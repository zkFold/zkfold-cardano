module ZkFold.Cardano.Rollup.Transaction.Update (Transaction(..), rollupUpdate) where

import           Cardano.Api                             (getScriptData, prettyPrintJSON)
import           Cardano.Api.Shelley                     (scriptDataFromJsonDetailedSchema, toPlutusData)
import           Control.Exception                       (throwIO)
import           Control.Monad                           (forM_)
import           Data.Aeson                              (decode, decodeFileStrict)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.Char                               (isUpper, toLower)
import           Data.Coerce                             (coerce)
import           Data.Maybe                              (fromJust)
import           Data.Set                                (lookupMin)
import           GeniusYield.GYConfig                    (GYCoreConfig (cfgNetworkId), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V1.Value                (Lovelace (..), lovelaceValue)
import           PlutusLedgerApi.V3                      (BuiltinByteString, Redeemer (..), ToData (..), TokenName (..),
                                                          dataToBuiltinData, fromData, toData)
import           Prelude
import           System.Directory                        (listDirectory, renameFile)
import           System.FilePath                         ((</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Algebra.EllipticCurve.BLS12_381  (Fr)
import           ZkFold.Cardano.Atlas.Utils              (SubmittedTx (..), readTxIdsFromFile, wrapUpSubmittedTx, wrapUpAndAppendSubmittedTx)
import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..), stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (dataToJSON)
import           ZkFold.Cardano.OnChain.BLS12_381        (F (..), toInput)
import           ZkFold.Cardano.OnChain.Utils            (dataToBlake)
import           ZkFold.Cardano.Options.Common           (CoreConfigAlt, SigningKeyAlt, dataOut, fromCoreConfigAltIO, fromSigningKeyAltIO)
import           ZkFold.Cardano.Rollup.Data              (bridgeOut, evolve, rollupFee)
import           ZkFold.Cardano.UPLC.Common              (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.Rollup              (RollupInfo (..), RollupRedeemer (..), RollupSetup (..))
import           ZkFold.Cardano.UPLC.RollupData          (RollupDataRedeemer (..), rollupDataCompiled)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CoreConfigAlt
  , requiredSigner :: !SigningKeyAlt
  , changeAddress  :: !GYAddress
  , rollupParkOut  :: !FilePath
  , dataParkOut    :: !FilePath
  , updateOut      :: !FilePath
  }

-- | Data token-name and redeemer from data update bytestring elements.
dataTokenInput :: [BuiltinByteString] -> (GYTokenName, GYRedeemer)
dataTokenInput dat = (tokenName, redeemer)
  where
    tokenName = fromJust . tokenNameFromPlutus . TokenName $ dataToBlake dat
    redeemer  = redeemerFromPlutus' . toBuiltinData $ NewData dat

-- | Produce skeletons for minting data tokens.
dataTokenSkeletons :: GYPubKeyHash                ->
                      -- ^ Pub key hash for change address.
                      GYAddress                   ->
                      -- ^ Address receiving the data tokens.
                      GYTxId                      ->
                      -- ^ TxId of parked 'rollupData' script.
                      [(GYTokenName, GYRedeemer)] ->
                      -- ^ Token name and redeemer for data token minting.
                      [GYTxSkeleton PlutusV3]
dataTokenSkeletons pkh dataAddr parkedTxId tuples =
  let rollupData = scriptFromPlutus @PlutusV3 $ rollupDataCompiled

      dataOref = txOutRefFromTuple (parkedTxId, 0)
      dataRef  = GYBuildPlutusScriptReference @PlutusV3 dataOref rollupData

      mustProduceToken :: GYTokenName -> GYRedeemer -> GYTxSkeleton PlutusV3
      mustProduceToken tn red = mustHaveOutput (GYTxOut dataAddr dataValue unitDatum' Nothing)
                             <> mustMint (GYBuildPlutusScript dataRef) red tn 1
                             <> mustBeSignedBy pkh
        where
          dataToken  = GYToken (mintingPolicyId rollupData) tn
          dataValue  = valueSingleton dataToken 1
          unitDatum' = Just (unitDatum, GYTxOutUseInlineDatum)

  in uncurry mustProduceToken <$> tuples

-- | Compute next rollup info.
nextRollup :: GYNetworkId -> Fr -> Integer -> RollupInfo -> IO RollupInfo
nextRollup nid x parkingTag rollupInfo = do
    ps <- generate arbitrary

    let dataUpdate1 = riDataUpdate rollupInfo
        state1      = riState      rollupInfo

    dataUpdate2 <- mapM evolve dataUpdate1

    let update2 = dataToBlake <$> dataUpdate2
        state2  = toInput $ dataToBlake (state1, update2, [snd $ bridgeOut nid parkingTag], lovelaceValue rollupFee)

        (_, _, proof2)  = stateCheckVerificationBytes x ps state2
        rollupRedeemer2 = UpdateRollup proof2 update2

    return $ RollupInfo parkingTag dataUpdate2 state2 rollupRedeemer2

-- | Produce rollup update skeleton.
rollupSkeleton :: GYNetworkId       ->
                  GYPubKeyHash      ->
                  -- ^ Pub key hash for change address.
                  Integer           ->
                  -- ^ Parking tag
                  GYTxId            ->
                  -- ^ TxId of 'rollup' parked script.
                  GYScript PlutusV3 ->
                  -- ^ Parameterized 'rollup' validator.
                  GYValue           ->
                  -- ^ Thread value.
                  F                 ->
                  -- ^ State.
                  RollupRedeemer    ->
                  -- ^ Rollup redeemer.
                  GYAddress         ->
                  -- ^ Fee address.
                  [GYTxId]            ->
                  -- ^ TxIds of minted data tokens.
                  GYTxOutRef        ->
                  -- ^ TxOutRef of thread token.
                  GYTxSkeleton PlutusV3
rollupSkeleton nid pkh parkingTag rollupTxId rollup threadValue state' redeemer' feeAddr
               dataTokenTxIds rollupIn = 

  let rollupOref = txOutRefFromTuple (rollupTxId, 0)
      rollupRef  = GYBuildPlutusScriptReference @PlutusV3 rollupOref rollup
      rollupAddr = addressFromValidator nid rollup

      F state     = state'
      inlineDatum = Just (datumFromPlutusData state, GYTxOutUseInlineDatum @PlutusV3)
      redeemer    = redeemerFromPlutus . Redeemer . dataToBuiltinData $ toData redeemer'

      dataRefs = (\dat -> txOutRefFromTuple (dat, 0)) <$> dataTokenTxIds
      
  in     mustHaveInput (GYTxIn rollupIn (GYTxInWitnessScript rollupRef Nothing redeemer))
      <> mconcat (mustHaveRefInput <$> dataRefs)
      <> mustHaveOutput (GYTxOut rollupAddr threadValue inlineDatum Nothing)
      <> mustHaveOutput (GYTxOut feeAddr (valueFromLovelace $ coerce rollupFee) Nothing Nothing)
      <> mustHaveOutput (fst $ bridgeOut nid parkingTag)
      <> mustBeSignedBy pkh

-- | Transactions for rollup update.
rollupUpdate :: Transaction -> IO ()
rollupUpdate (Transaction path coreCfg' sig changeAddr rollupParkOut dataParkOut updateOut) = do
  let testData = path </> "test-data"
      assets   = path </> "assets"

  rollup <- readScript (assets </> "rollup.script")

  rollupSetupE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assets </> "rollupSetup.json")
  rollupInfoE  <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assets </> "rollupInfo.json")

  let rollupScriptDataE = do
        rollupSetup' <- rollupSetupE
        rollupInfo'  <- rollupInfoE
        return (rollupSetup', rollupInfo')

  case rollupScriptDataE of
    Right (rollupSetupScriptData, rollupInfoScriptData) -> do
      coreCfg  <- fromCoreConfigAltIO coreCfg'
      skey     <- fromSigningKeyAltIO sig

      let nid        = cfgNetworkId coreCfg
          rollupAddr = addressFromScript nid rollup

      IdentityCircuitContract x _ <- fromJust . decode <$> BL.readFile (testData </> "plonk-raw-contract-data.json")

      let rollupSetup = fromJust . fromData . toPlutusData . getScriptData $ rollupSetupScriptData :: RollupSetup
          rollupInfo  = fromJust . fromData . toPlutusData . getScriptData $ rollupInfoScriptData :: RollupInfo

      let RollupSetup _ _ threadValue' feeAddr'                     = rollupSetup
          RollupInfo parkingTag dataUpdate nextState rollupRedeemer = rollupInfo

      newRollupInfo <- nextRollup nid x parkingTag rollupInfo
      threadValue   <- valueFromPlutusIO threadValue'
      feeAddr       <- either (throwIO . userError . show) pure $ addressFromPlutus nid feeAddr'

      let threadToken = case lookupMin . valueAssets . valueNonAda $ threadValue of
                          Just tk -> tk
                          Nothing -> error "Missing thread token."

      let dataTokenInputs = dataTokenInput <$> dataUpdate

      BS.writeFile (assets </> "newRollupInfo.json") $ prettyPrintJSON $ dataToJSON newRollupInfo

      let parkingSpot = scriptFromPlutus @PlutusV3 $ parkingSpotCompiled parkingTag
          parkingAddr = addressFromValidator nid parkingSpot

      rollupTxId     <- decodeFileStrict (assets </> rollupParkOut)
                        >>= maybe (fail $ "Failed to decode " ++ rollupParkOut) pure
      rollupDataTxId <- decodeFileStrict (assets </> dataParkOut)
                        >>= maybe (fail $ "Failed to decode " ++ dataParkOut) pure

      let w1 = User' skey Nothing changeAddr

      pkh <- addressToPubKeyHashIO changeAddr
      
      let dataSkeletons = dataTokenSkeletons pkh
                                             parkingAddr
                                             rollupDataTxId
                                             dataTokenInputs
          rollSkeleton  = rollupSkeleton nid
                                         pkh
                                         parkingTag
                                         rollupTxId
                                         rollup
                                         threadValue
                                         nextState
                                         rollupRedeemer
                                         feeAddr

      BS.writeFile (assets </> dataOut) BS.empty

      withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
        forM_ dataSkeletons $ \sk -> (runGYTxGameMonadIO nid
                                                         providers $
                                                         asUser w1 $ do
                                                           txbody <- buildTxBody sk
                                                           txid   <- signAndSubmitConfirmed txbody
                                                           return $ SubmittedTx txid (Just $ txBodyFee txbody))
                                     >>= wrapUpAndAppendSubmittedTx (assets </> dataOut)

        dataTokenTxIds <- readTxIdsFromFile (assets </> dataOut)

        threadUtxos <- runGYTxQueryMonadIO nid
                                           providers
                                           (utxosAtAddress rollupAddr (Just threadToken))

        case utxosRefs threadUtxos of
              [rollupIn] -> do
                tx2 <- runGYTxGameMonadIO nid
                                          providers $
                                          asUser w1 $ do
                                            txbody <- buildTxBody $ rollSkeleton dataTokenTxIds rollupIn
                                            txid   <- signAndSubmitConfirmed txbody
                                            return $ SubmittedTx txid (Just $ txBodyFee txbody)

                wrapUpSubmittedTx (assets </> updateOut) tx2

                renameNewFiles assets

              _ -> throwIO $ userError "Missing utxo with thread token."
    Left _  -> throwIO $ userError  "JSON error: unreadable rollup script data."


----- HELPER FUNCTIONS -----

renameNewFiles :: FilePath -> IO ()
renameNewFiles dir = do
  files <- listDirectory dir
  let matching = filter isNewFile files
  forM_ matching $ \file -> do
    let newName = toTargetName file
    renameFile (dir </> file) (dir </> newName)

-- | Check if file name starts with "new" and the next character is uppercase
isNewFile :: String -> Bool
isNewFile ('n':'e':'w':c:_) = isUpper c
isNewFile _                 = False

-- | Converts "newFileName" to "fileName" (lowercase first letter)
toTargetName :: String -> String
toTargetName ('n':'e':'w':c:rest) = toLower c : rest
toTargetName name                 = name
