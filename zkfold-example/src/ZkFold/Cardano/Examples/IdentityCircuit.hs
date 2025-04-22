module ZkFold.Cardano.Examples.IdentityCircuit where

import           Data.Aeson                                  (FromJSON, ToJSON)
import           GHC.Generics                                (Generic, Par1 (..))
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import qualified Prelude                                     as Haskell

import           ZkFold.Algebra.Class             (zero)
import           ZkFold.Algebra.Field             (toZp)
import           ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, Fr)
import           ZkFold.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Protocol.Plonkup                (Plonkup (..))
import           ZkFold.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)
import           ZkFold.Protocol.Plonkup.Utils          (getParams, getSecrectParams)
import           ZkFold.Protocol.Plonkup.Witness        (PlonkupWitnessInput (..))
import           ZkFold.Cardano.OffChain.Plonkup             (PlonkupN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.OnChain.BLS12_381.F          (F (..))
import           ZkFold.Cardano.OnChain.Plonkup              (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data         (InputBytes, ProofBytes, SetupBytes)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), eval, idCircuit)

data IdentityCircuitContract = IdentityCircuitContract {
    x'  :: Fr
  , ps' :: PlonkupProverSecret BLS12_381_G1_Point
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

identityCircuit :: ArithmeticCircuit Fr Par1 Par1
identityCircuit = idCircuit

identityCircuitVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1_Point -> (SetupBytes, InputBytes, ProofBytes)
identityCircuitVerificationBytes x ps =
    let (omega, k1, k2) = getParams 2
        witnessInputs   = eval identityCircuit $ Par1 zero
        (gs, h1)        = getSecrectParams x
        plonkup         = Plonkup omega k1 k2 identityCircuit h1 gs :: PlonkupN Par1 2 Par1
        setupP          = setupProve plonkup
        setupV          = setupVerify plonkup
        witness         = (PlonkupWitnessInput @_ @BLS12_381_G1_Point witnessInputs, ps)
        (input, proof)  = prove @(PlonkupN Par1 2 Par1) setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

stateCheckVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1_Point -> F -> (SetupBytes, InputBytes, ProofBytes)
stateCheckVerificationBytes x ps state =
    let F n             = state
        state'          = toZp n :: Fr
        (omega, k1, k2) = getParams 2
        witnessInputs   = Par1 state'
        (gs, h1)        = getSecrectParams x
        plonkup         = Plonkup omega k1 k2 identityCircuit h1 gs :: PlonkupN Par1 2 Par1
        setupP          = setupProve plonkup
        setupV          = setupVerify plonkup
        witness         = (PlonkupWitnessInput witnessInputs, ps)
        (input, proof)  = prove @(PlonkupN Par1 2 Par1) setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

testIdentityCircuit :: Fr -> PlonkupProverSecret BLS12_381_G1_Point -> Haskell.Bool
testIdentityCircuit x ps =
    let (s, i, p) = identityCircuitVerificationBytes x ps
    in verify @PlonkupPlutus s i p
