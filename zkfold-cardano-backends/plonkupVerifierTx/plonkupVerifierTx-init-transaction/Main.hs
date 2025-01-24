module Main where

import           Data.Aeson                            (encode)
import qualified Data.ByteString.Lazy                  as BL
import           Prelude                               (Bool (..), IO, Show (..), putStr, ($), (++))
import           System.Directory                      (createDirectoryIfMissing)
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)

import           ZkFold.Cardano.Examples.EqualityCheck (EqualityCheckContract (..), equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils         (savePlutus)
import           ZkFold.Cardano.UPLC.ForwardingScripts (forwardingRewardCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx   (plonkupVerifierTxCompiled)

main :: IO ()
main = do
  x           <- generate arbitrary
  ps          <- generate arbitrary
  targetValue <- generate arbitrary

  let contract = EqualityCheckContract x ps targetValue

  createDirectoryIfMissing True "../../test-data"
  createDirectoryIfMissing True "../../assets"

  BL.writeFile "../../test-data/plonkupVerifierTx-raw-contract-data.json" $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n"

  let (setup, _, _) = equalityCheckVerificationBytes x ps targetValue

  savePlutus "../../assets/plonkupVerifierTx.plutus" $ plonkupVerifierTxCompiled setup
  savePlutus "../../assets/forwardingReward.plutus" forwardingRewardCompiled
