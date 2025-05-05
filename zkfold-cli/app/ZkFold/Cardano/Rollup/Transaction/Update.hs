module ZkFold.Cardano.Rollup.Transaction.Update (Transaction(..), rollupUpdate) where

import           Cardano.Api                    (getScriptData, prettyPrintJSON)
import           Cardano.Api.Shelley            (scriptDataFromJsonDetailedSchema, toPlutusData)
import           Control.Exception              (throwIO)
import           Control.Monad                  (forM_)
import           Data.Aeson                     (decode, decodeFileStrict)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toLower, isUpper)
import           Data.Coerce                    (coerce)
import           Data.Maybe                     (fromJust)
import           Data.Set                       (lookupMin)
import           GeniusYield.GYConfig           (GYCoreConfig (cfgNetworkId), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V1.Value       (Lovelace(..), lovelaceValue)
import           PlutusLedgerApi.V3             (BuiltinByteString, Redeemer (..), ToData (..), TokenName (..),
                                                 dataToBuiltinData, fromData, toData)
import           Prelude
import           System.Directory               (listDirectory, renameFile)
import           System.FilePath                ((</>))
import qualified System.IO                      as IO
import           Test.QuickCheck.Arbitrary                   (Arbitrary (..))
import           Test.QuickCheck.Gen                         (generate)
import           Text.Read                      (readMaybe)

import           ZkFold.Algebra.EllipticCurve.BLS12_381  (Fr)
import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..), stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils  (dataToJSON)
import           ZkFold.Cardano.OnChain.BLS12_381        (F (..), toInput)
import           ZkFold.Cardano.OnChain.Utils   (dataToBlake)
import           ZkFold.Cardano.Options.Common  (CoreConfigAlt, HasFileParser (..), SigningKeyAlt, StageTx (..), SubmittedTx (..),
                                                 fromCoreConfigAltIO, fromSigningKeyAltIO, wrapUpSubmittedTx)
import           ZkFold.Cardano.Rollup.Data     (bridgeOut, evolve, rollupFee)
import           ZkFold.Cardano.UPLC.Common     (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.Rollup     (RollupInfo (..), RollupRedeemer (..), RollupSetup (..))
import           ZkFold.Cardano.UPLC.RollupData (RollupDataRedeemer (..), rollupDataCompiled)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CoreConfigAlt
  , requiredSigner :: !SigningKeyAlt
  , changeAddress  :: !GYAddress
  , initOutFile    :: !FilePath
  , outFileData    :: !FilePath
  , outFileUpdate  :: !FilePath
  }

-- | Data token-name and redeemer from data update bytestring elements.
dataTokenInput :: [BuiltinByteString] -> (GYTokenName, GYRedeemer)
dataTokenInput dat = (tokenName, redeemer)
  where
    tokenName = fromJust . tokenNameFromPlutus . TokenName $ dataToBlake dat
    redeemer  = redeemerFromPlutus' . toBuiltinData $ NewData dat

-- | Produce skeleton for minting data tokens.
dataTokenSkeleton :: GYAddress                   ->
                     -- ^ Change address for wallet funding this Tx.
                     GYAddress                   ->
                     -- ^ Address receiving the data tokens.
                     GYTxId                      ->
                     -- ^ TxId of parked scripts.
                     [(GYTokenName, GYRedeemer)] ->
                     -- ^ Token name and redeemer for data token minting.
                     Maybe (GYTxSkeleton PlutusV3)
dataTokenSkeleton changeAddr dataAddr parkedTxId tuples =
  let rollupData = scriptFromPlutus @PlutusV3 $ rollupDataCompiled
  
      dataOref = txOutRefFromTuple (parkedTxId, 1)
      dataRef  = GYBuildPlutusScriptReference @PlutusV3 dataOref rollupData

      mustProduceToken :: GYTokenName -> GYRedeemer -> GYTxSkeleton PlutusV3
      mustProduceToken tn red = mustHaveOutput (GYTxOut dataAddr dataValue unitDatum' Nothing)
                             <> mustMint (GYBuildPlutusScript dataRef) red tn 1
        where
          dataToken  = GYToken (mintingPolicyId rollupData) tn
          dataValue  = valueSingleton dataToken 1
          unitDatum' = Just (unitDatum, GYTxOutUseInlineDatum)
  in do
    pkh <- addressToPubKeyHash changeAddr
    return $ mconcat (uncurry mustProduceToken <$> tuples)
          <> mustBeSignedBy pkh

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

-- | Produce rollup update skeleton as a function of the thread token's TxOutRef.
rollupSkeleton :: GYNetworkId       ->
                  GYAddress         ->
                  -- ^ Change address for wallet funding this Tx.
                  GYTxId            ->
                  -- ^ TxId of parked scripts.
                  RollupRedeemer    ->
                  -- ^ Rollup redeemer.
                  GYScript PlutusV3 ->
                  -- ^ Parameterized 'rollup' validator.
                  [GYTxOutRef]      ->
                  -- ^ TxOut references to data tokens.
                  GYValue           ->
                  -- ^ Thread value.
                  F                 ->
                  -- ^ State.
                  GYAddress         ->
                  -- ^ Fee address.
                  Integer           ->
                  -- ^ Parking tag
                  Maybe (GYTxOutRef -> GYTxSkeleton PlutusV3)
rollupSkeleton nid changeAddr parkedTxId redeemer' rollup dataRefs threadValue state' feeAddr parkingTag =
  let rollupRefOref = txOutRefFromTuple (parkedTxId, 0)
      rollupRef     = GYBuildPlutusScriptReference @PlutusV3 rollupRefOref rollup
      rollupAddr    = addressFromValidator nid rollup

      F state     = state'
      inlineDatum = Just (datumFromPlutusData state, GYTxOutUseInlineDatum @PlutusV3)
      redeemer    = redeemerFromPlutus . Redeemer . dataToBuiltinData $ toData redeemer'
  in do
    pkh <- addressToPubKeyHash changeAddr
    return $ \rollupIn ->
               mustHaveInput (GYTxIn rollupIn (GYTxInWitnessScript rollupRef Nothing redeemer))
            <> mconcat (mustHaveRefInput <$> dataRefs)
            <> mustHaveOutput (GYTxOut rollupAddr threadValue inlineDatum Nothing)
            <> mustHaveOutput (GYTxOut feeAddr (valueFromLovelace $ coerce rollupFee) Nothing Nothing)
            <> mustHaveOutput (fst $ bridgeOut nid parkingTag)
            <> mustBeSignedBy pkh

-- | Transactions for rollup update.
rollupUpdate :: Transaction -> IO ()
rollupUpdate (Transaction path coreCfg' sig changeAddr initOut dataOut updateOut) = do
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

      let RollupSetup _ _ threadValue' feeAddr'                                  = rollupSetup
          RollupInfo parkingTag dataUpdate nextState rollupRedeemer@(UpdateRollup _ update) = rollupInfo

      newRollupInfo <- nextRollup nid x parkingTag rollupInfo
      threadValue   <- valueFromPlutusIO threadValue'
      feeAddr       <- either (throwIO . userError . show) pure $ addressFromPlutus nid feeAddr'

      let threadToken = case lookupMin . valueAssets . valueNonAda $ threadValue of
                          Just tk -> tk
                          Nothing -> error "Missing thread token."

      let dataTokenInputs = dataTokenInput <$> dataUpdate

      BS.writeFile (assets </> "newRollupInfo.json") $ prettyPrintJSON $ dataToJSON newRollupInfo
      IO.writeFile (assets </> "newDataTokensAmount.txt") . show . length $ update

      let parkingSpot = scriptFromPlutus @PlutusV3 $ parkingSpotCompiled parkingTag
          parkingAddr = addressFromValidator nid parkingSpot

      initTxId     <- decodeFileStrict (assets </> initOut)
                      >>= maybe (fail $ "Failed to decode " ++ outFileName RollupInit) pure
      dataTokensTx <- decodeFileStrict (assets </> "dataTokens.tx")
                      >>= maybe (fail $ "Failed to decode " ++ outFileName RollupData) pure
      tokensAmount <- IO.readFile (assets </> "dataTokensAmount.txt")
                      >>= maybe (fail "Failed to parse dataTokensAmount.txt") pure . readMaybe @Word

      let dataRefs = (\n -> txOutRefFromTuple (dataTokensTx, n)) <$> [0..tokensAmount - 1]

      let w1 = User' skey Nothing changeAddr

      let mSkeletonTuple = do
            skeleton1 <- dataTokenSkeleton changeAddr
                                           parkingAddr
                                           initTxId
                                           dataTokenInputs
            skeleton2F <- rollupSkeleton nid
                                         changeAddr
                                         initTxId
                                         rollupRedeemer
                                         rollup
                                         dataRefs
                                         threadValue
                                         nextState
                                         feeAddr
                                         parkingTag
            return (skeleton1, skeleton2F)

      case mSkeletonTuple of
        Just (skeleton1, skeleton2F) -> do
          withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
            tx1 <- runGYTxGameMonadIO nid
                                      providers $
                                      asUser w1 $ do
                                        txbody <- buildTxBody skeleton1
                                        txid   <- signAndSubmitConfirmed txbody
                                        return $ SubmittedTx txid (Just $ txBodyFee txbody)

            wrapUpSubmittedTx (assets </> dataOut) tx1

            threadUtxos <- runGYTxQueryMonadIO nid
                                               providers
                                               (utxosAtAddress rollupAddr (Just threadToken))

            case utxosRefs threadUtxos of
                  [rollupIn] -> do
                    tx2 <- runGYTxGameMonadIO nid
                                              providers $
                                              asUser w1 $ do
                                                txbody <- buildTxBody $ skeleton2F rollupIn
                                                txid   <- signAndSubmitConfirmed txbody
                                                return $ SubmittedTx txid (Just $ txBodyFee txbody)

                    wrapUpSubmittedTx (assets </> updateOut) tx2

                    renameNewFiles assets

                  _ -> throwIO $ userError "Missing utxo with thread token."
        Nothing -> throwIO $ userError "Unable to parse change address"
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
toTargetName name = name
