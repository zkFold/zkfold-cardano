module ZkFold.Cardano.PlonkupVerifierTx.Transaction.Tx where

import           Cardano.Api                             (runExceptT)
import           Cardano.CLI.EraBased.Script.Read.Common (readScriptDataOrFile)
import           Cardano.CLI.Type.Common                 (TxOutAnyEra (..))
import           Control.Exception                       (throwIO)
import           Control.Monad                           (forM, when)
import           Data.Aeson                              (decode, encodeFile)
import qualified Data.ByteString.Lazy                    as BL
import           Data.Maybe                              (fromJust, isJust)
import           GeniusYield.GYConfig                    (GYCoreConfig (cfgNetworkId), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3                      as V3
import qualified PlutusTx.Builtins.Internal              as BI
import           PlutusTx.Prelude                        (blake2b_224, sortBy)
import           Prelude
import           System.FilePath                         ((</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Atlas.Utils              (txOutFromApi, txOutToPlutus, utxoToTxInInfo)
import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (outRefCompare)
import           ZkFold.Cardano.OnChain.BLS12_381        (toInput)
import           ZkFold.Cardano.Options.Common
import           ZkFold.Cardano.PlonkupVerifierTx.Types
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx   (plonkupVerifierTxCompiled)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CoreConfigAlt
  , requiredSigner :: !SigningKeyAlt
  , changeAddress  :: !GYAddress
  , txInputs       :: ![TxInputInfo]
  , txRefs         :: ![GYTxOutRef]
  , txOuts         :: ![TxOutAnyEra]
  , submitTx       :: !Bool
  , outFile        :: !FilePath
  }

alwaysValid :: Interval POSIXTime
alwaysValid = Interval (LowerBound (NegInf :: Extended POSIXTime) True) (UpperBound PosInf True)

sortTxInInfoList :: [TxInInfo] -> [TxInInfo]
sortTxInInfoList = sortBy $ \u v -> outRefCompare (txInInfoOutRef u) (txInInfoOutRef v)

txInFromInfo :: TxInputInfo -> IO (GYTxIn PlutusV3)
txInFromInfo (TxInputInfo oref input) = do
  case input of
    ScriptInput scriptFile sd refOref' -> do
      script <- readScript @PlutusV3 scriptFile

      let plutusScript = case refOref' of
            ScriptRefOref refOref -> GYBuildPlutusScriptReference @PlutusV3 refOref script
            NotScriptRef          -> GYBuildPlutusScriptInlined @PlutusV3 script

      redeemerDataE <- runExceptT $ readScriptDataOrFile sd

      redeemer <- case redeemerDataE of
        Left err           -> throwIO . userError $ show err
        Right redeemerData -> pure $ redeemerFromApi redeemerData

      let wit = GYTxInWitnessScript plutusScript Nothing redeemer

      return $ GYTxIn oref wit

    PubKeyInput -> return $ GYTxIn oref GYTxInWitnessKey

verifierTx :: Transaction -> IO ()
verifierTx (Transaction path coreCfg' sig changeAddr ins gyRefs outs doSubmit outFile) = do
  let assets    = path </> "assets"
      setupFile = assets </> "plonkupVerifierTx-setup-data.json"

  coreCfg <- fromCoreConfigAltIO coreCfg'
  skey    <- fromSigningKeyAltIO sig

  let nid = cfgNetworkId coreCfg
      w1  = User' skey Nothing changeAddr

  PlonkupVerifierTxSetup x <- fromJust . decode <$> BL.readFile setupFile
  ps <- generate arbitrary

  let (setup, _, _) = identityCircuitVerificationBytes x ps

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    gyTxIns  <- forM ins txInFromInfo
    gyTxOuts <- forM outs txOutFromApi

    let skeleton' = mconcat (mustHaveInput <$> gyTxIns)
                 <> mconcat (mustHaveRefInput <$> gyRefs)
                 <> mconcat (mustHaveOutput <$> gyTxOuts)

    mUtxosIn  <- forM (txinOref <$> ins) $ runGYTxQueryMonadIO nid providers . utxoAtTxOutRef
    mUtxosRef <- forM gyRefs $ runGYTxQueryMonadIO nid providers . utxoAtTxOutRef

    when (not $ all isJust mUtxosIn) . throwIO $ userError "No UTxO found for some input."
    when (not $ all isJust mUtxosRef) . throwIO $ userError "No UTxO found for some reference input."

    let utxosIn  = fromJust <$> mUtxosIn
        utxosRef = fromJust <$> mUtxosRef

    let plutusValidator = plonkupVerifierTxCompiled setup
        script          = scriptFromPlutus @PlutusV3 plutusValidator
        scriptAddr      = addressFromValidator nid script
        plutusScript    = GYBuildPlutusScriptInlined @PlutusV3 script

    utxosAtScript <- runGYTxQueryMonadIO nid
                                         providers
                                         (utxosAtAddress scriptAddr Nothing)

    let selectedOref = fst . fromJust $ someTxOutRef utxosAtScript
        selectedUtxo = fromJust $ utxosLookup selectedOref utxosAtScript

    let ins'  = sortTxInInfoList $ utxoToTxInInfo <$> (selectedUtxo : utxosIn)
        refs' = sortTxInInfoList $ utxoToTxInInfo <$> utxosRef
        outs' = txOutToPlutus <$> gyTxOuts
        outsN = fromIntegral $ length outs' :: Integer

    let insBD    = toBuiltinData ins'
        refsBD   = toBuiltinData refs'
        outsBD   = toBuiltinData outs'
        rangeBD  = toBuiltinData alwaysValid  -- ToDo: generalize to bounded intervals
        txDataBD = mkTuple4 insBD refsBD outsBD rangeBD

    let input = toInput . blake2b_224 . BI.serialiseData $ txDataBD

    let (_, _, proof) = stateCheckVerificationBytes x ps input

    let redeemer = redeemerFromPlutusData (proof, outsN)
        wit      = GYTxInWitnessScript plutusScript Nothing redeemer

    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = skeleton'
                <> mustHaveInput (GYTxIn @PlutusV3 selectedOref wit)
                <> mustBeSignedBy pkh

    txbody <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1
                                 (buildTxBody skeleton)

    putStr $ "\nEstimated transaction fee: " ++ (show $ txBodyFee txbody) ++ " Lovelace\n"

    when doSubmit $ do
      txid <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1
                                 (signAndSubmitConfirmed txbody)

      putStr $ "Transaction Id: " ++ show txid ++ "\n\n"
      encodeFile (assets </> outFile) txid


----- HELPER FUNCTIONS -----

mkTuple4 :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData
mkTuple4 a b c d =
  BI.mkList $
    BI.mkCons a $
      BI.mkCons b $
        BI.mkCons c $
          BI.mkCons d $
            BI.mkNilData BI.unitval
