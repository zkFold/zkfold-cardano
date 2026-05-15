module ZkFold.Cardano.Asterizm.Transaction.Hash where

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base16       as B16
import qualified Data.Text.Encoding           as TE
import           PlutusLedgerApi.V3           (fromBuiltin, toBuiltin)
import           Prelude

import           ZkFold.Cardano.UPLC.Asterizm (buildCrosschainHash, buildHash)


data Transaction = Transaction
  { message :: !BS.ByteString
  }

computeHash :: Transaction -> IO ()
computeHash = computeBuildCrosschainHash

computeBuildCrosschainHash :: Transaction -> IO ()
computeBuildCrosschainHash (Transaction msg) = do
  let msgHash = fromBuiltin . buildCrosschainHash . toBuiltin $ msg
      hexHash = TE.decodeUtf8 $ B16.encode msgHash
  putStrLn $ show hexHash

computeBuildHash :: Transaction -> IO ()
computeBuildHash (Transaction msg) = do
  let msgHash = fromBuiltin . buildHash . toBuiltin $ msg
      hexHash = TE.decodeUtf8 $ B16.encode msgHash
  putStrLn $ show hexHash
