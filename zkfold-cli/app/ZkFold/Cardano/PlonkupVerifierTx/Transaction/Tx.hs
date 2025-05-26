module ZkFold.Cardano.PlonkupVerifierTx.Transaction.Tx where

import           Cardano.Api                           (SerialiseAsRawBytes (..), parsePolicyId, prettyPrintJSON, runExceptT)
import           Cardano.Api.Ledger                    (toCBOR)
import           Cardano.Api.Shelley                   (toPlutusData)
import           Cardano.CLI.Type.Common               (ReferenceScriptAnyEra (..), ScriptDataOrFile, TxOutAnyEra (..), TxOutDatumAnyEra (..))
import           Cardano.CLI.EraBased.Script.Read.Common (readScriptDataOrFile)
import           Codec.CBOR.Write                      (toStrictByteString)
import           Control.Exception                     (throwIO)
import           Control.Monad                         (forM, when)
import           Data.Aeson                            (decode, encode)
import           Data.ByteString                       as BS (writeFile)
import qualified Data.ByteString.Lazy                  as BL
-- import           Data.Coerce                           (coerce)
import           Data.Maybe                            (fromJust, isJust)
import           GeniusYield.GYConfig                  (GYCoreConfig (cfgNetworkId), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
-- import           PlutusLedgerApi.V1                    (TxOutRef (..))
import qualified PlutusLedgerApi.V2                    as V2
import           PlutusLedgerApi.V3                    as V3
-- import           PlutusLedgerApi.V3                    (BuiltinData, OutputDatum (..), ScriptHash, ToData, TxInInfo (..), TxOut (..), TxOutRef (..), UnsafeFromData,
--                                                         toBuiltinData, unsafeFromBuiltinData, unsafeFromData)
import qualified PlutusTx.Builtins.Internal            as BI
import           PlutusTx.Prelude                      (blake2b_224, sortBy)
import           Prelude
-- import           Prelude                               (Bool (..), Either (..), FilePath, IO, Show (..), String, error,
--                                                         head, ($), (++), (.), (<$>))
import           System.Directory                      (createDirectoryIfMissing)
import           System.FilePath                       ((</>))
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)
import           Text.Parsec                           (parse)

import           ZkFold.Cardano.Examples.EqualityCheck (EqualityCheckContract (..), equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils         (dataToJSON, outRefCompare, savePlutus)
import           ZkFold.Cardano.OnChain.BLS12_381      (toInput)
import           ZkFold.Cardano.Options.Common
-- import           ZkFold.Cardano.Options.Common         (CoreConfigAlt, SigningKeyAlt, TxInputInfo (..), fromCoreConfigAltIO,
--                                                         fromSigningKeyAltIO)
import           ZkFold.Cardano.UPLC.ForwardingScripts (forwardingRewardCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx (plonkupVerifierTxCompiled)


data Transaction = Transaction
--  { curPath        :: !FilePath
  { coreCfgAlt :: !CoreConfigAlt
  , txInVerify :: !GYTxOutRef
  , txInputs   :: ![TxInputInfo]
  , txRefs     :: ![GYTxOutRef]
  , txOuts     :: ![TxOutAnyEra]
  }

txInFromInfo :: TxInputInfo -> IO (GYTxIn PlutusV3)
txInFromInfo (TxInputInfo oref input) = do
  case input of
    ScriptInput scriptFile sd refOref -> do
      script <- readScript @PlutusV3 scriptFile

      let plutusScript = case refOref of
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
    _ -> throwIO $ userError "Unsupported datum specification."

  case refS of
    ReferenceScriptAnyEraNone -> return $ GYTxOut addr' val' dat' Nothing
    _ -> throwIO $ userError "Output with reference script is not supported."

-- convertBetweenPlutusVersions :: (ToData a, FromData b) => a -> b
-- convertBetweenPlutusVersions = fromJust . fromBuiltinData . toBuiltinData

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
verifierTx (Transaction coreCfg' _inVerify ins gyRefs outs) = do
  coreCfg  <- fromCoreConfigAltIO coreCfg'

  let nid = cfgNetworkId coreCfg

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    gyTxIns  <- forM ins txInFromInfo
    gyTxOuts <- forM outs txOutFromApi

    let skeleton = mconcat (mustHaveInput <$> gyTxIns)
                <> mconcat (mustHaveRefInput <$> gyRefs)
                <> mconcat (mustHaveOutput <$> gyTxOuts)

    mUtxosIn  <- forM (txinOref <$> ins) $ runGYTxQueryMonadIO nid providers . utxoAtTxOutRef
    mUtxosRef <- forM gyRefs $ runGYTxQueryMonadIO nid providers . utxoAtTxOutRef

    when (not $ all isJust mUtxosIn) . throwIO $ userError "No UTxO found for some input."
    when (not $ all isJust mUtxosRef) . throwIO $ userError "No UTxO found for some reference input."

    let ins'  = sortTxInInfoList $ utxoToTxInInfo . fromJust <$> mUtxosIn
        refs' = sortTxInInfoList $ utxoToTxInInfo . fromJust <$> mUtxosRef
        outs' = txOutToPlutus <$> gyTxOuts

    let insBD    = toBuiltinData ins'
        refsBD   = toBuiltinData refs'
        outsBD   = toBuiltinData outs'
        rangeBD  = toBuiltinData alwaysValid  -- ToDo: generalize to bounded intervals
        txDataBD = mkTuple4 insBD refsBD outsBD rangeBD

    let input = toInput . blake2b_224 . BI.serialiseData $ txDataBD

    putStr $ "Verifier's input: " ++ (show input) ++ "\n\n"



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
