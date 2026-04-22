module ZkFold.Cardano.Asterizm.Transaction.Policy where

import qualified Data.ByteString                              as BS
import           GeniusYield.Types
import           Prelude

import           ZkFold.Cardano.Asterizm.Transaction.Retrieve (derivePolicyId)
import           ZkFold.Cardano.Asterizm.Types                (MessageDirection (..))
import           ZkFold.Cardano.Asterizm.Utils                (policyFromPlutus)
import           ZkFold.Cardano.Options.Common                (readPaymentVerificationKey)
import           ZkFold.Cardano.UPLC.Asterizm                 (asterizmRelayerCompiled, asterizmUserCompiled)


data ClientTransaction = ClientTransaction
  { clientVKeyFile   :: !FilePath
  , relayerVKeyFiles :: ![FilePath]
  , trustedAddresses :: ![BS.ByteString]
  , direction        :: !MessageDirection
  }

data RelayerTransaction = RelayerTransaction
  { relayerVKeyFile :: !FilePath
  }

data UserTransaction = UserTransaction

printClientPolicy :: ClientTransaction -> IO ()
printClientPolicy (ClientTransaction clientVkeyFile relayerVkeyFiles trustedAddressBSs dir) = do
  clientVkey   <- readPaymentVerificationKey clientVkeyFile
  relayerVkeys <- mapM readPaymentVerificationKey relayerVkeyFiles

  let clientPolicyId = derivePolicyId clientVkey relayerVkeys trustedAddressBSs dir
  putStrLn $ trimQuot (show clientPolicyId)

printRelayerPolicy :: RelayerTransaction -> IO ()
printRelayerPolicy (RelayerTransaction relayerVkeyFile) = do
  vkey <- readPaymentVerificationKey relayerVkeyFile
  let policyId = snd . policyFromPlutus . asterizmRelayerCompiled . pubKeyHashToPlutus . pubKeyHash $ vkey
  putStrLn $ trimQuot (show policyId)

printUserPolicy :: UserTransaction -> IO ()
printUserPolicy UserTransaction = do
  let policyId = snd . policyFromPlutus $ asterizmUserCompiled
  putStrLn $ trimQuot (show policyId)

-- | Remove enclosing quotation marks
trimQuot :: String -> String
trimQuot = reverse . drop 1 . reverse . drop 1
