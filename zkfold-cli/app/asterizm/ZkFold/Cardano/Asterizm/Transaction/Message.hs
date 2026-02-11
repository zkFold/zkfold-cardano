module ZkFold.Cardano.Asterizm.Transaction.Message where

import           Data.Aeson                    (encodeFile)
import qualified Data.ByteString               as BS
import           PlutusLedgerApi.V3            (fromBuiltin, toBuiltin)
import           Prelude
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (AsterizmMessage (..))
import           ZkFold.Cardano.UPLC.Asterizm  (buildCrosschainHash)


data Transaction = Transaction
  { curPath     :: !FilePath
  , message     :: !BS.ByteString
  , privateFile :: !FilePath
  , publicFile  :: !FilePath
  }

clientMessage :: Transaction -> IO ()
clientMessage (Transaction path msg privFile pubFile) = do
  let assetsPath = path </> "assets"

  let msgHash = fromBuiltin . buildCrosschainHash . toBuiltin $ msg

  putStrLn $ "\nSaving Asterizm message (private file: " ++ privFile ++ ")..."
  encodeFile (assetsPath </> privFile) $ AsterizmMessage msg

  putStrLn $ "\nSaving message hash (public file: " ++ pubFile ++ ")..."
  encodeFile (assetsPath </> pubFile) $ AsterizmMessage msgHash

  putStrLn "\nDone."
