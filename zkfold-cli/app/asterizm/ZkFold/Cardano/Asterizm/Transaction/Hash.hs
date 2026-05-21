module ZkFold.Cardano.Asterizm.Transaction.Hash where

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base16       as B16
import qualified Data.Text.Encoding           as TE
import           Prelude

import           ZkFold.Cardano.Asterizm.Utils (hashMessage)
import           ZkFold.Cardano.UPLC.Asterizm  (AsterizmHashMode)


data Transaction = Transaction
  { hashMode :: !AsterizmHashMode
  , message  :: !BS.ByteString
  }

computeHash :: Transaction -> IO ()
computeHash (Transaction mode msg) = do
  let msgHash = hashMessage mode msg
      hexHash = TE.decodeUtf8 $ B16.encode msgHash
  putStrLn $ show hexHash
