{-# LANGUAGE GADTs #-}

module ZkFold.Cardano.Atlas.Utils where

import           Cardano.Api                             hiding (TxOut)
import           Cardano.CLI.EraBased.Script.Read.Common (readScriptDataOrFile)
import           Cardano.CLI.Type.Common                 (ReferenceScriptAnyEra (..), ScriptDataOrFile,
                                                          TxOutAnyEra (..), TxOutDatumAnyEra (..))
import           Control.Exception                       (throwIO)
import           GeniusYield.Types
import           PlutusLedgerApi.V3                      as V3
import           Prelude

import           ZkFold.Cardano.CLI.Utils                (bumpTxOutRef)


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
