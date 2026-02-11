module ZkFold.Cardano.Asterizm.Transaction.Init where

import           Data.Aeson                    (encode)
import qualified Data.ByteString.Lazy          as BL
import           GeniusYield.Types             (pubKeyHashToPlutus)
import           Prelude
import           System.Directory              (createDirectoryIfMissing, doesFileExist)
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (AsterizmSetup (..))
import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import           ZkFold.Cardano.Options.Common
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmRelayerCompiled)


data Transaction = Transaction
  { curPath     :: !FilePath
  , clientPKH   :: !PubKeyHashAlt
  , relayerPKHs :: ![PubKeyHashAlt]
  }

asterizmInit :: Transaction -> IO ()
asterizmInit (Transaction path clientPKHA relayerPKHAs) = do
  let assetsPath = path </> "assets"
      setupFile  = assetsPath </> "asterizm-setup.json"

  createDirectoryIfMissing True assetsPath
  setupFileExists <- doesFileExist setupFile

  if setupFileExists
    then putStr $ "\nSetup file " ++ setupFile ++ " already exists.\n\n"
    else do
      clientPKH   <- fromPubKeyHashAltIO clientPKHA
      relayerPKHs <- mapM fromPubKeyHashAltIO relayerPKHAs

      let relayerPolicyIds = snd . policyFromPlutus
                             . asterizmRelayerCompiled . pubKeyHashToPlutus <$> relayerPKHs

      let asterizmSetup = AsterizmSetup
            { acsClientPKH       = clientPKH
            , acsAllowedRelayers = relayerPolicyIds
            }

      BL.writeFile setupFile $ encode asterizmSetup

      putStr $ "\nSetup file " ++ setupFile ++ " created.\n"
      putStrLn ("Client PKH: " ++ show clientPKH)
      putStr $ "Allowed relayers: " ++ show (length relayerPolicyIds) ++ " relayer(s)\n\n"
