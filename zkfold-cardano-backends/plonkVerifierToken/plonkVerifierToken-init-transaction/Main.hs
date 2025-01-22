module Main where

import           Prelude                                (Bool (..), IO, ($))
import           System.Directory                       (createDirectoryIfMissing)
import           Test.QuickCheck.Arbitrary              (Arbitrary (..))
import           Test.QuickCheck.Gen                    (generate)

import           ZkFold.Cardano.Examples.EqualityCheck  (equalityCheckContract)
import           ZkFold.Cardano.OffChain.Utils          (savePlutus)
import           ZkFold.Cardano.UPLC.ForwardingScripts  (forwardingMintCompiled)
import           ZkFold.Cardano.UPLC.PlonkVerifierToken (plonkVerifierTokenCompiled)
import           GHC.Generics                                (Par1 (..), U1 (..), type (:*:) (..))
import           ZkFold.Cardano.OffChain.Plonkup             (PlonkupN, mkSetup)
import           ZkFold.Base.Protocol.NonInteractiveProof    (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams)
import           ZkFold.Base.Protocol.Plonkup                (Plonkup (..))
import ZkFold.Prelude (writeFileJSON)
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)

main :: IO ()
main = do
  -- create secret constants for PlonkUp
  x           <- generate arbitrary
  ps          <- generate arbitrary
  targetValue <- generate arbitrary

  -- environment check
  createDirectoryIfMissing True "../test-data"
  createDirectoryIfMissing True "../assets"

  -- make a setup from an arithmetic circuit
  let -- сompiling your arithmetic circuit to PlonkUp
      ac = compile @Fr (equalityCheckContract @Fr @(ArithmeticCircuit Fr (U1 :*: U1) (Par1 :*: U1)) targetValue) :: ArithmeticCircuit Fr (U1 :*: U1) (Par1 :*: U1) Par1
      (omega, k1, k2) = getParams 32
      plonkup = Plonkup omega k1 k2 ac x :: PlonkupN (U1 :*: U1) (Par1 :*: U1) 32
      -- creating a setup for a smart contract
      setupV  = setupVerify @_ @HaskellCore plonkup
      setup   = mkSetup setupV
      -- creating a setup for prover
      setupP  = setupProve @_ @HaskellCore plonkup

  -- use different label to get another forwardingMint
  let fmLabel = 0

  -- save the things needed for the prover
  -- writeFileJSON "../assets/setupProve.json" setupP
  writeFileJSON "../assets/plonkupProverSecret.json" (ps :: PlonkupProverSecret BLS12_381_G1)
  
  -- this part only exists in the example, this is a cheat sheet
  writeFileJSON "../assets/privateInputs.json" targetValue

  -- save the smart contracts
  savePlutus "../assets/plonkVerifierToken.plutus" $ plonkVerifierTokenCompiled setup
  savePlutus "../assets/forwardingMint.plutus" $ forwardingMintCompiled fmLabel
