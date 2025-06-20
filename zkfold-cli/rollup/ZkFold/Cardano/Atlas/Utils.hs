{-# LANGUAGE GADTs #-}

module ZkFold.Cardano.Atlas.Utils where

import           Cardano.Api            hiding (TxOut)
import           Control.Monad          (when)
import           Data.Aeson             (encodeFile)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as BS8
import           Data.Either.Extra      (eitherToMaybe)
import           Data.Maybe             (fromJust, isJust, mapMaybe)
import           GeniusYield.Types
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
