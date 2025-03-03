module ZkFold.Cardano.Examples.IdentityCircuit where

import           Data.Aeson                                  (FromJSON, ToJSON)
import           GHC.Generics                                (Generic, Par1 (..), U1 (..), type (:*:) (..))
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import qualified Prelude                                     as Haskell

import           ZkFold.Base.Algebra.Basic.Class             (zero)
import           ZkFold.Base.Algebra.Basic.Field             (toZp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, BLS12_381_G2_Point, Fr)
import           ZkFold.Base.Protocol.NonInteractiveProof    (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonkup                (Plonkup (..))
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams, getSecrectParams)
import           ZkFold.Base.Protocol.Plonkup.Witness        (PlonkupWitnessInput (..))
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

identityCircuit :: ArithmeticCircuit Fr (U1 :*: U1) Par1 Par1
identityCircuit = idCircuit

identityCircuitVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1_Point -> (SetupBytes, InputBytes, ProofBytes)
identityCircuitVerificationBytes x ps =
    let (omega, k1, k2) = getParams 16384
        witnessInputs   = eval identityCircuit (U1 :*: U1) $ Par1 zero
        (gs, h1)        = getSecrectParams x
        plonkup         = Plonkup omega k1 k2 identityCircuit h1 gs :: PlonkupN (U1 :*: U1) Par1 2
        setupP          = setupProve @_ @HaskellCore plonkup
        setupV          = setupVerify @_ @HaskellCore plonkup
        witness         = (PlonkupWitnessInput @_ @_ @BLS12_381_G1_Point (U1 :*: U1) witnessInputs, ps)
        (input, proof)  = prove @(PlonkupN (U1 :*: U1) Par1 16384) @HaskellCore setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

stateCheckVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1_Point -> F -> (SetupBytes, InputBytes, ProofBytes)
stateCheckVerificationBytes x ps state =
    let F n             = state
        state'          = toZp n :: Fr
        (omega, k1, k2) = getParams 16384
        (gs, h1) = getSecrectParams @16384 @BLS12_381_G1_Point @BLS12_381_G2_Point x
        witnessInputs   = Par1 state'
<<<<<<< HEAD
<<<<<<< HEAD
        (gs, h1)        = getSecrectParams x
=======
>>>>>>> 0f58e9d (Fixed errors; using mock zkLogin)
        plonkup         = Plonkup omega k1 k2 identityCircuit h1 gs :: PlonkupN (U1 :*: U1) Par1 2
=======
        plonkup         = Plonkup omega k1 k2 identityCircuit h1 gs :: PlonkupN (U1 :*: U1) Par1 16384
>>>>>>> 11a5ece (WIP redeemer constructor)
        setupP          = setupProve @_ @HaskellCore plonkup
        setupV          = setupVerify @_ @HaskellCore plonkup
        witness         = (PlonkupWitnessInput (U1 :*: U1) witnessInputs, ps)
        (input, proof)  = prove @(PlonkupN (U1 :*: U1) Par1 16384) @HaskellCore setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

testIdentityCircuit :: Fr -> PlonkupProverSecret BLS12_381_G1_Point -> Haskell.Bool
testIdentityCircuit x ps =
    let (s, i, p) = identityCircuitVerificationBytes x ps
    in verify @PlonkupPlutus @HaskellCore s i p
