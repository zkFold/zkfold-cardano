module ZkFold.Cardano.Asterizm.Transaction.Message where

import           Data.Aeson                    (encodeFile)
import qualified Data.ByteString               as BS
import           PlutusCore.Crypto.Hash        (blake2b_256)
import           Prelude
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (HexByteString (..))


data Transaction = Transaction
  { curPath     :: !FilePath
  , message     :: !BS.ByteString
  , privateFile :: !FilePath
  , publicFile  :: !FilePath
  }

clientMessage :: Transaction -> IO ()
clientMessage (Transaction path msg privFile pubFile) = do
  let assetsPath = path </> "assets"

  let msgHash = blake2b_256 msg

  putStrLn $ "\nSaving Asterizm message (private file: " ++ privFile ++ ")..."
  encodeFile (assetsPath </> privFile) $ HexByteString msg

  putStrLn $ "\nSaving message hash (public file: " ++ pubFile ++ ")..."
  encodeFile (assetsPath </> pubFile) $ HexByteString msgHash

  putStrLn "\nDone."
