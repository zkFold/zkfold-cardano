module ZkFold.Cardano.Rollup.Transaction.Init (Transaction(..), rollupInit) where

import           Cardano.Api                             (prettyPrintJSON)
import           Control.Exception                       (throwIO)
import           Data.Aeson                              (encode)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.Set                                (lookupMin)
import           Data.String                             (IsString (..))
import           GeniusYield.GYConfig                    (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V1.Value                (lovelaceValue)
import qualified PlutusLedgerApi.V2                      as V2
import qualified PlutusLedgerApi.V3                      as V3
import           Prelude
import           System.FilePath                         ((</>))
import           System.Random                           (randomRIO)
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (currencySymbolOf, dataToJSON)
import           ZkFold.Cardano.OnChain.BLS12_381        (F (..), bls12_381_field_prime, toInput)
import           ZkFold.Cardano.OnChain.Plonkup.Data     (SetupBytes)
import           ZkFold.Cardano.OnChain.Utils            (dataToBlake)
import           ZkFold.Cardano.Options.Common           (CoreConfigAlt, SigningKeyAlt, SubmittedTx (..),
                                                          fromCoreConfigAltIO, fromSigningKeyAltIO, wrapUpSubmittedTx)
import           ZkFold.Cardano.Rollup.Data              (bridgeOut, rmax, rollupFee, threadLovelace, updateLength)
import           ZkFold.Cardano.UPLC.Common              (nftPolicyCompiled, parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.Rollup              (RollupInfo (..), RollupRedeemer (..), RollupSetup (..),
                                                          rollupCompiled)
import           ZkFold.Cardano.UPLC.RollupData          (rollupDataCompiled)


data Transaction = Transaction
    { curPath        :: !FilePath
    , coreCfgAlt     :: !CoreConfigAlt
    , requiredSigner :: !SigningKeyAlt
    , changeAddress  :: !GYAddress
    , nftOref        :: !GYTxOutRef
    , feeAddress     :: !GYAddress
    , outFile        :: !FilePath
    }

nftMintingPolicy :: GYTxOutRef -> GYScript PlutusV3
nftMintingPolicy nftOref = scriptFromPlutus $ nftPolicyCompiled nftOref'
  where
    nftOref' = bumpTxOutRef $ txOutRefToPlutus nftOref

rollupScript :: SetupBytes  ->
                -- ^ Ledger rules.
                GYTxOutRef  ->
                -- ^ TxOutRef parameterizing thread token.
                GYTokenName ->
                -- ^ Thread token's name.
                GYAddress   ->
                -- ^ Fee address.
                (RollupSetup, GYScript PlutusV3)
rollupScript setup nftOref nftName feeAddr =
  let nftPolicyId = mintingPolicyId $ nftMintingPolicy nftOref
      nftValue    = valueSingleton (GYToken nftPolicyId nftName) 1

      rollupSetup = RollupSetup
        { rsLedgerRules  = setup
        , rsDataCurrency = currencySymbolOf rollupDataCompiled
        , rsThreadValue  = lovelaceValue threadLovelace <> valueToPlutus nftValue
        , rsFeeAddress   = addressToPlutus feeAddr
        }
  in (rollupSetup, scriptFromPlutus $ rollupCompiled rollupSetup)

initStateSkeleton :: GYNetworkId       ->
                     GYAddress         ->
                     -- ^ Change address for wallet funding this Tx.
                     GYTxOutRef        ->
                     -- ^ TxOutRef parameterizing thread token.
                     GYValue           ->
                     -- ^ Thread value.
                     GYScript PlutusV3 ->
                     -- ^ Rollup validator.
                     F                 ->
                     -- ^ Initial state.
                     Maybe (GYTxSkeleton PlutusV3)
initStateSkeleton nid changeAddr nftOref threadValue rollup iniState =
  let nftName = case lookupMin . valueAssets . valueNonAda $ threadValue of
                  Just (GYToken _ t) -> t
                  _                  -> error "Thread nft not found."

      inlineDatum = Just (datumFromPlutusData iniState, GYTxOutUseInlineDatum @PlutusV3)

      nftPolicy'  = nftMintingPolicy nftOref
      nftPolicy   = GYBuildPlutusScript $ GYBuildPlutusScriptInlined nftPolicy'

      rollupAddr  = addressFromValidator nid rollup
  in do
    pkh <- addressToPubKeyHash changeAddr
    return $ mustHaveInput (GYTxIn nftOref GYTxInWitnessKey)
          <> mustHaveOutput (GYTxOut rollupAddr threadValue inlineDatum Nothing)
          <> mustMint nftPolicy unitRedeemer nftName 1
          <> mustBeSignedBy pkh

parkRollupSkeleton :: GYNetworkId ->
                      GYAddress ->
                      -- ^ Change address for wallet funding this Tx.
                      Integer ->
                      -- ^ Parking tag
                      GYScript PlutusV3 ->
                      -- ^ Rollup validator.
                      Maybe (GYTxSkeleton PlutusV3)
parkRollupSkeleton nid changeAddr tag rollup =
  let parkingSpot = scriptFromPlutus @PlutusV3 $ parkingSpotCompiled tag
      parkingAddr = addressFromValidator nid parkingSpot

      rollupData = scriptFromPlutus $ rollupDataCompiled

      validators = Just . GYPlutusScript <$> [rollup, rollupData]
      valTxOuts  = GYTxOut parkingAddr mempty Nothing <$> validators
  in do
    pkh <- addressToPubKeyHash changeAddr
    return $ mconcat (mustHaveOutput <$> valTxOuts)
          <> mustBeSignedBy pkh

rollupInit :: Transaction -> IO ()
rollupInit (Transaction path coreCfg' sig changeAddr nftOref feeAddress outFile) = do
    let testData = path </> "test-data"
        assets   = path </> "assets"

    coreCfg  <- fromCoreConfigAltIO coreCfg'
    skey     <- fromSigningKeyAltIO sig

    let nid = cfgNetworkId coreCfg

    parkingTag <- randomRIO (1, 10000)
    x          <- generate arbitrary
    ps         <- generate arbitrary
    seeds      <- mapM (\_ -> randomRIO (1, rmax)) [1..updateLength]
    iniState'  <- randomRIO (0, bls12_381_field_prime - 1)

    let contract            = IdentityCircuitContract x ps
    BL.writeFile (testData </> "plonk-raw-contract-data.json") $ encode contract

    let nftName  = GYTokenName (fromString "zkFold" :: BS.ByteString)

    let (ledgerRules, _, _)   = identityCircuitVerificationBytes x ps
        (rollupSetup, rollup) = rollupScript ledgerRules nftOref nftName feeAddress

    let RollupSetup _ _ threadValue' _ = rollupSetup
    threadValue <- valueFromPlutusIO threadValue'

    BS.writeFile (path </> "rollupSetup.json") $ prettyPrintJSON $ dataToJSON rollupSetup
    writeScript (assets </> "rollup.script") rollup

    let dataUpdate = fmap (\s -> [dataToBlake s]) seeds
        update     = dataToBlake <$> dataUpdate
        iniState   = F iniState'

        nextState     = toInput $ dataToBlake (iniState, update, [snd $ bridgeOut nid parkingTag], lovelaceValue rollupFee)
        (_, _, proof) = stateCheckVerificationBytes x ps nextState

    let rollupRedeemer = UpdateRollup proof update
        rollupInfo     = RollupInfo parkingTag dataUpdate nextState rollupRedeemer
    BS.writeFile (assets </> "rollupInfo.json") $ prettyPrintJSON $ dataToJSON rollupInfo

    putStr "\nDone serializing plutus scripts and initializing state.\n\n"

    let w1 = User' skey Nothing changeAddr

    let mskeleton = do
          initState  <- initStateSkeleton nid changeAddr nftOref threadValue rollup iniState
          parkRollup <- parkRollupSkeleton nid changeAddr parkingTag rollup
          return $ initState <> parkRollup

    case mskeleton of
      Just skeleton -> withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
        tx <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1 $ do
                                   txbody <- buildTxBody skeleton
                                   txid   <- signAndSubmitConfirmed txbody
                                   return $ SubmittedTx txid (Just $ txBodyFee txbody)

        wrapUpSubmittedTx outFile tx

      Nothing -> throwIO $ userError "Unable to parse change address"


----- HELPER FUNCTIONS -----

bumpTxId :: V2.TxId -> V3.TxId
bumpTxId (V2.TxId a) = V3.TxId a

bumpTxOutRef :: V2.TxOutRef -> V3.TxOutRef
bumpTxOutRef (V2.TxOutRef a b) = V3.TxOutRef (bumpTxId a) b
