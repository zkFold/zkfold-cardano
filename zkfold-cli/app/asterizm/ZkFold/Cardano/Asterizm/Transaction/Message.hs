module ZkFold.Cardano.Asterizm.Transaction.Message where

import           Control.Exception             (throwIO)
import           Data.Aeson                    (encodeFile)
import qualified Data.ByteString               as BS
import           PlutusLedgerApi.V3            (fromBuiltin, toBuiltin)
import           Prelude
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (AsterizmMessageHash (..), fromByteString, toByteString)
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

  -- Parse raw bytes into structured message
  structuredMsg <- case fromByteString msg of
    Just m  -> pure m
    Nothing -> throwIO $ userError "Message too short: requires at least 112-byte header."

  -- Compute hash from raw bytes
  let msgHash = fromBuiltin . buildCrosschainHash . toBuiltin . toByteString $ structuredMsg

  putStrLn $ "\nSaving Asterizm message (private file: " ++ privFile ++ ")..."
  encodeFile (assetsPath </> privFile) structuredMsg

  putStrLn $ "\nSaving message hash (public file: " ++ pubFile ++ ")..."
  encodeFile (assetsPath </> pubFile) $ AsterizmMessageHash msgHash

  putStrLn "\nDone."
