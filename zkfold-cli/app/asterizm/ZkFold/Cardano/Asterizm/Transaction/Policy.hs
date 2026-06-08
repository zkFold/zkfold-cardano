module ZkFold.Cardano.Asterizm.Transaction.Policy where

import qualified Data.ByteString                              as BS
import           GeniusYield.Types
import           Prelude

import           ZkFold.Cardano.Asterizm.Transaction.Retrieve (derivePolicyId)
import           ZkFold.Cardano.Asterizm.Utils                (policyFromPlutus)
import           ZkFold.Cardano.Options.Common                (readPaymentVerificationKey)
import           ZkFold.Cardano.UPLC.Asterizm                 (asterizmOmniTokenCompiled, asterizmRelayerCompiled,
                                                               asterizmUserCompiled)


data ClientTransaction = ClientTransaction
  { clientVKeyFile   :: !FilePath
  , relayerVKeyFiles :: ![FilePath]
  , trustedAddresses :: ![BS.ByteString]
  }

data RelayerTransaction = RelayerTransaction
  { relayerVKeyFile :: !FilePath
  }

data UserTransaction = UserTransaction

data TokenTransaction = TokenTransaction
  { tokenClientVKeyFile   :: !FilePath
  , tokenRelayerVKeyFiles :: ![FilePath]
  , tokenTrustedAddresses :: ![BS.ByteString]
  }

printClientPolicy :: ClientTransaction -> IO ()
printClientPolicy (ClientTransaction clientVkeyFile relayerVkeyFiles trustedAddressBSs) = do
  clientVkey   <- readPaymentVerificationKey clientVkeyFile
  relayerVkeys <- mapM readPaymentVerificationKey relayerVkeyFiles

  let clientPolicyId = derivePolicyId clientVkey relayerVkeys trustedAddressBSs
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

printTokenPolicy :: TokenTransaction -> IO ()
printTokenPolicy (TokenTransaction clientVkeyFile relayerVkeyFiles trustedAddressBSs) = do
  clientVkey   <- readPaymentVerificationKey clientVkeyFile
  relayerVkeys <- mapM readPaymentVerificationKey relayerVkeyFiles

  let clientPolicyId = derivePolicyId clientVkey relayerVkeys trustedAddressBSs
      userPolicyId = snd . policyFromPlutus $ asterizmUserCompiled
      clientCS = mintingPolicyIdToCurrencySymbol clientPolicyId
      userCS = mintingPolicyIdToCurrencySymbol userPolicyId
      tokenPolicyId = snd . policyFromPlutus $ asterizmOmniTokenCompiled clientCS userCS
  putStrLn $ trimQuot (show tokenPolicyId)

-- | Remove enclosing quotation marks
trimQuot :: String -> String
trimQuot = reverse . drop 1 . reverse . drop 1
