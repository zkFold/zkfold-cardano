{-# LANGUAGE GADTs #-}

module ZkFold.Cardano.Atlas.Utils where

import           Cardano.Api                             hiding (TxOut)
import           Cardano.CLI.EraBased.Script.Read.Common (readScriptDataOrFile)
import           Cardano.CLI.Type.Common                 (ReferenceScriptAnyEra (..), ScriptDataOrFile,
                                                          TxOutAnyEra (..), TxOutDatumAnyEra (..))
import           Control.Exception                       (throwIO)
import           Control.Monad                           (when)
import           Data.Aeson                              (encodeFile)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Base16                  as B16
import qualified Data.ByteString.Char8                   as BS8
import           Data.Either.Extra                       (eitherToMaybe)
import           Data.Maybe                              (fromJust, isJust, mapMaybe)
import           GeniusYield.Types
import qualified PlutusLedgerApi.V2                      as V2
import           PlutusLedgerApi.V3                      as V3
import           Prelude

---------------------------- :write/read TxIds: ----------------------------

appendTxIdToFile :: FilePath -> GYTxId -> IO ()
appendTxIdToFile file txid = do
  let raw    = serialiseToRawBytes $ txIdToApi txid
      hexBS  = B16.encode raw <> BS8.pack "\n"

  BS.appendFile file hexBS

readTxIdsFromFile :: FilePath -> IO [GYTxId]
readTxIdsFromFile file = do
  contents <- BS.readFile file
  let linesBS = BS8.lines contents
  return $ mapMaybe parseHexTxId linesBS

parseHexTxId :: BS.ByteString -> Maybe GYTxId
parseHexTxId hex = do
  raw   <- eitherToMaybe $ B16.decode hex
  txid' <- eitherToMaybe $ deserialiseFromRawBytes AsTxId raw
  return $ txIdFromApi txid'

---------------------------------- :Tx: ----------------------------------

data SubmittedTx = SubmittedTx
  { stTxId  :: !GYTxId
  , stTxFee :: !(Maybe Integer)
  } deriving stock Show

wrapUpSubmittedTx' :: (FilePath -> GYTxId -> IO ()) -> FilePath  -> SubmittedTx -> IO ()
wrapUpSubmittedTx' f outFile SubmittedTx{..} = do
  putStr "\n"

  when (isJust stTxFee) $
    putStr $ "Estimated transaction fee: " ++ (show $ fromJust stTxFee) ++ " Lovelace\n"

  putStr $ "Transaction Id: " ++ show stTxId ++ "\n\n"

  f outFile stTxId

wrapUpSubmittedTx, wrapUpAndAppendSubmittedTx :: FilePath -> SubmittedTx -> IO ()

wrapUpSubmittedTx          = wrapUpSubmittedTx' encodeFile
wrapUpAndAppendSubmittedTx = wrapUpSubmittedTx' appendTxIdToFile

----------------------------- :Conversions: ------------------------------

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

------------------------------- :Helpers: --------------------------------

bumpTxId :: V2.TxId -> V3.TxId
bumpTxId (V2.TxId a) = V3.TxId a

bumpTxOutRef :: V2.TxOutRef -> V3.TxOutRef
bumpTxOutRef (V2.TxOutRef a b) = V3.TxOutRef (bumpTxId a) b
