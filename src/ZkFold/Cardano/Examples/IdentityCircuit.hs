module ZkFold.Cardano.Examples.IdentityCircuit where

import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import qualified Prelude                                     as Haskell

import           ZkFold.Base.Algebra.Basic.Class             (zero)
import           ZkFold.Base.Algebra.Basic.Field             (toZp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Data.Vector                     (Vector (..), item, singleton, unsafeToVector)
import           ZkFold.Base.Protocol.NonInteractiveProof    (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonkup                (Plonkup (..))
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams)
import           ZkFold.Base.Protocol.Plonkup.Witness        (PlonkupWitnessInput (..))
import           ZkFold.Cardano.OffChain.Plonkup             (PlonkupN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.OnChain.BLS12_381.F          (F (..))
import           ZkFold.Cardano.OnChain.Plonkup              (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data         (InputBytes, ProofBytes, SetupBytes)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile, eval)

identityCircuit :: ArithmeticCircuit Fr (Vector 1) (Vector 1)
identityCircuit = compile (id :: ArithmeticCircuit Fr (Vector 1) (Vector 1) -> ArithmeticCircuit Fr (Vector 1) (Vector 1))

identityCircuitVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1 -> (SetupBytes, InputBytes, ProofBytes)
identityCircuitVerificationBytes x ps =
    let (omega, k1, k2) = getParams 2
        witnessInputs   = unsafeToVector [item $ eval identityCircuit $ singleton zero]
        plonkup         = Plonkup omega k1 k2 identityCircuit x :: PlonkupN 1 2
        setupP          = setupProve @_ @HaskellCore plonkup
        setupV          = setupVerify @_ @HaskellCore plonkup
        witness         = (PlonkupWitnessInput witnessInputs, ps)
        (input, proof)  = prove @(PlonkupN 1 2) @HaskellCore setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

stateCheckVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1 -> F -> (SetupBytes, InputBytes, ProofBytes)
stateCheckVerificationBytes x ps state =
    let F n             = state
        state'          = toZp n :: Fr
        (omega, k1, k2) = getParams 2
        witnessInputs   = unsafeToVector [state']
        plonkup         = Plonkup omega k1 k2 identityCircuit x :: PlonkupN 1 2
        setupP          = setupProve @_ @HaskellCore plonkup
        setupV          = setupVerify @_ @HaskellCore plonkup
        witness         = (PlonkupWitnessInput witnessInputs, ps)
        (input, proof)  = prove @(PlonkupN 1 2) @HaskellCore setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

testIdentityCircuit :: Fr -> PlonkupProverSecret BLS12_381_G1 -> Haskell.Bool
testIdentityCircuit x ps =
    let (s, i, p) = identityCircuitVerificationBytes x ps
    in verify @PlonkupPlutus @HaskellCore s i p
