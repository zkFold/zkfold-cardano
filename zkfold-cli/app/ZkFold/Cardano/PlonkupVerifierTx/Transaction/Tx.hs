{-# LANGUAGE GADTs #-}

module ZkFold.Cardano.PlonkupVerifierTx.Transaction.Tx where

import           Cardano.Api                             (runExceptT)
import           Cardano.CLI.EraBased.Script.Read.Common (readScriptDataOrFile)
import           Cardano.CLI.Type.Common                 (ReferenceScriptAnyEra (..), ScriptDataOrFile,
                                                          TxOutAnyEra (..), TxOutDatumAnyEra (..))
import           Control.Exception                       (throwIO)
import           Control.Monad                           (forM, when)
import           Data.Aeson                              (decode)
import qualified Data.ByteString.Lazy                    as BL
import           Data.Maybe                              (fromJust, isJust)
import           GeniusYield.GYConfig                    (GYCoreConfig (cfgNetworkId), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import qualified PlutusLedgerApi.V2                      as V2
import           PlutusLedgerApi.V3                      as V3
import qualified PlutusTx.Builtins.Internal              as BI
import           PlutusTx.Prelude                        (blake2b_224, sortBy)
import           Prelude
import           System.FilePath                         ((</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

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
  }

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

datumFromScriptData :: ScriptDataOrFile -> Bool -> IO (GYDatum, GYTxOutUseInlineDatum PlutusV3)
datumFromScriptData sd useInlineDatum = do
  let gyUseInlineDatum = if useInlineDatum
        then GYTxOutUseInlineDatum
        else GYTxOutDontUseInlineDatum

  datumE <- runExceptT $ readScriptDataOrFile sd

  case datumE of
    Left err  -> throwIO . userError $ show err
    Right dat -> pure (datumFromApi' dat, gyUseInlineDatum)

txOutFromApi :: TxOutAnyEra -> IO (GYTxOut PlutusV3)
txOutFromApi (TxOutAnyEra addr val dat refS) = do
  let addr' = addressFromApi addr
      val'  = valueFromApi val

  dat' <- case dat of
    TxOutDatumByHashOf sd      -> Just <$> datumFromScriptData sd False
    TxOutDatumByValue sd       -> Just <$> datumFromScriptData sd False
    TxOutInlineDatumByValue sd -> Just <$> datumFromScriptData sd True
    TxOutDatumByNone           -> pure Nothing
    _                          -> throwIO $ userError "Unsupported datum specification."

  case refS of
    ReferenceScriptAnyEraNone -> return $ GYTxOut addr' val' dat' Nothing
    _                         -> throwIO $ userError "Output with reference script is not supported."

utxoToTxInInfo :: GYUTxO -> TxInInfo
utxoToTxInInfo u = TxInInfo txOutRef txOut
  where
    txOutRef = bumpTxOutRef . txOutRefToPlutus $ utxoRef u
    txOut    = utxoToPlutus u

txOutToPlutus :: GYTxOut PlutusV3 -> TxOut
txOutToPlutus out =
  let txOutAddress = addressToPlutus $ gyTxOutAddress out
      txOutValue   = valueToPlutus $ gyTxOutValue out

      txOutDatum = case gyTxOutDatum out of
        Nothing                               -> NoOutputDatum
        Just (dat, GYTxOutDontUseInlineDatum) -> OutputDatumHash . datumHashToPlutus $ hashDatum dat
        Just (dat, GYTxOutUseInlineDatum)     -> OutputDatum $ datumToPlutus dat

      txOutReferenceScript = scriptHashToPlutus . hashAnyScript <$> gyTxOutRefS out

  in TxOut txOutAddress txOutValue txOutDatum txOutReferenceScript

sortTxInInfoList :: [TxInInfo] -> [TxInInfo]
sortTxInInfoList = sortBy $ \u v -> outRefCompare (txInInfoOutRef u) (txInInfoOutRef v)

alwaysValid :: Interval POSIXTime
alwaysValid = Interval (LowerBound (NegInf:: Extended POSIXTime) True) (UpperBound PosInf True)

verifierTx :: Transaction -> IO ()
verifierTx (Transaction path coreCfg' sig changeAddr ins gyRefs outs) = do
  let assetsPath = path </> "assets"
      setupFile  = assetsPath </> "plonkupVerifierTx-setup-data.json"

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

    let insBD    = toBuiltinData ins'
        refsBD   = toBuiltinData refs'
        outsBD   = toBuiltinData outs'
        rangeBD  = toBuiltinData alwaysValid  -- ToDo: generalize to bounded intervals
        txDataBD = mkTuple4 insBD refsBD outsBD rangeBD

    let input = toInput . blake2b_224 . BI.serialiseData $ txDataBD

    putStr $ "Verifier's input: " ++ (show input) ++ "\n\n"

    let (_, _, proof) = stateCheckVerificationBytes x ps input

    let redeemer = redeemerFromPlutusData proof
        wit      = GYTxInWitnessScript plutusScript Nothing redeemer

    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = skeleton'
                <> mustHaveInput (GYTxIn @PlutusV3 selectedOref wit)
                <> mustBeSignedBy pkh

    txbody <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1
                                 (buildTxBody skeleton)

    print txbody


----- HELPER FUNCTIONS -----

bumpTxId :: V2.TxId -> V3.TxId
bumpTxId (V2.TxId a) = V3.TxId a

bumpTxOutRef :: V2.TxOutRef -> V3.TxOutRef
bumpTxOutRef (V2.TxOutRef a b) = V3.TxOutRef (bumpTxId a) b

mkTuple4 :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData
mkTuple4 a b c d =
  BI.mkList $
    BI.mkCons a $
      BI.mkCons b $
        BI.mkCons c $
          BI.mkCons d $
            BI.mkNilData BI.unitval
