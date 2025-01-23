module Main where

import           Data.ByteString                          as BS (writeFile)
import           GHC.Generics                             (Par1 (..), U1 (..), type (:*:) (..))
import           Prelude                                  (IO, undefined, ($))

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonkup             ()
import           ZkFold.Base.Protocol.Plonkup.Witness     (PlonkupWitnessInput (..))
import           ZkFold.Cardano.OffChain.Plonkup          (PlonkupN, mkInput, mkProof)
import           ZkFold.Cardano.OffChain.Utils            (createTokenName, dataToCBOR)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F       as F
import           ZkFold.Cardano.OnChain.Plonkup           ()
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes (..))
import           ZkFold.Prelude                           (readFileJSON)

dummyRedeemer :: ProofBytes
dummyRedeemer = ProofBytes e e e e e e e e e e e e e 0 0 0 0 0 0 0 0 0 0 0 0 (F.F 0)
  where e = ""

main :: IO ()
main = do
  -- read the things needed for the prover
  -- setupP <- readFileJSON "../assets/setupProve.json" -- PlonkupProverSetup
  let setupP = undefined :: SetupProve (PlonkupN (U1 :*: U1) (Par1 :*: U1) 32)
  ps     <- readFileJSON "../assets/plonkupProverSecret.json"

  -- request private parameters
  targetValue <- readFileJSON "../assets/privateInputs.json"

  -- make a proof from a private inputs and PlonkUp secrets
  let witnessInputs  = Par1 targetValue :*: U1
      witness        = (PlonkupWitnessInput @_ @(Par1 :*: U1) @_ (U1 :*: U1) witnessInputs, ps)
      (input, proof) = prove @(PlonkupN (U1 :*: U1) (Par1 :*: U1) 32) @HaskellCore setupP witness

  -- creating data for smart contracts
  BS.writeFile "../assets/tokenname" $ createTokenName $ mkInput input
  BS.writeFile "../assets/redeemerPlonkVerifierToken.cbor" $ dataToCBOR $ mkProof proof

  -- creation of cheap elements
  BS.writeFile "../assets/unit.cbor" $ dataToCBOR ()
  BS.writeFile "../assets/dummy-redeemer.cbor" $ dataToCBOR dummyRedeemer
