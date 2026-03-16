module ZkFold.Cardano.Asterizm.Transaction.Policy where

import           GeniusYield.Types
import           Prelude

import           ZkFold.Cardano.Asterizm.Transaction.Retrieve (derivePolicyId)
import           ZkFold.Cardano.Asterizm.Types                (MessageDirection (..))
import           ZkFold.Cardano.Asterizm.Utils                (policyFromPlutus)
import           ZkFold.Cardano.Options.Common                (readPaymentVerificationKey)
import           ZkFold.Cardano.UPLC.Asterizm                 (asterizmRelayerCompiled)


data ClientTransaction = ClientTransaction
  { clientVKeyFile   :: !FilePath
  , relayerVKeyFiles :: ![FilePath]
  , direction        :: !MessageDirection
  }

data RelayerTransaction = RelayerTransaction
  { relayerVKeyFile :: !FilePath
  }

printClientPolicy :: ClientTransaction -> IO ()
printClientPolicy (ClientTransaction clientVkeyFile relayerVkeyFiles dir) = do
  clientVkey   <- readPaymentVerificationKey clientVkeyFile
  relayerVkeys <- mapM readPaymentVerificationKey relayerVkeyFiles

  let clientPolicyId = derivePolicyId clientVkey relayerVkeys dir
  putStrLn $ trimQuot (show clientPolicyId)

printRelayerPolicy :: RelayerTransaction -> IO ()
printRelayerPolicy (RelayerTransaction relayerVkeyFile) = do
  vkey <- readPaymentVerificationKey relayerVkeyFile
  let policyId = snd . policyFromPlutus . asterizmRelayerCompiled . pubKeyHashToPlutus . pubKeyHash $ vkey
  putStrLn $ trimQuot (show policyId)

-- | Remove enclosing quotation marks
trimQuot :: String -> String
trimQuot = reverse . drop 1 . reverse . drop 1
