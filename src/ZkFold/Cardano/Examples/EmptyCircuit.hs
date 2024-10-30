module ZkFold.Cardano.Examples.EmptyCircuit where

import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import qualified Prelude                                     as Haskell

import           ZkFold.Base.Algebra.Basic.Class             (negate, zero)
import           ZkFold.Base.Algebra.Basic.Field             (toZp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Data.Vector                     (Vector (..), item, singleton, unsafeToVector)
import           ZkFold.Base.Protocol.NonInteractiveProof    (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonk
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams)
import           ZkFold.Base.Protocol.Plonkup.Witness
import           ZkFold.Cardano.OffChain.Plonk               (PlonkN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.OnChain.BLS12_381            (F (..))
import           ZkFold.Cardano.OnChain.Plonk                (PlonkPlutus)
import           ZkFold.Cardano.OnChain.Plonk.Data           (InputBytes, ProofBytes, SetupBytes)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), eval, compile)

emptyCircuit :: ArithmeticCircuit Fr (Vector 1) (Vector 1)
emptyCircuit = compile (id :: ArithmeticCircuit Fr (Vector 1) (Vector 1) -> ArithmeticCircuit Fr (Vector 1) (Vector 1))

emptyCircuitVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1 -> (SetupBytes, InputBytes, ProofBytes)
emptyCircuitVerificationBytes x ps =
    let (omega, k1, k2) = getParams 2
        witnessInputs   = unsafeToVector [item $ eval emptyCircuit $ singleton zero]
        plonk           = Plonk omega k1 k2 emptyCircuit x :: PlonkN 1 2
        setupP          = setupProve @_ @HaskellCore plonk
        setupV          = setupVerify @_ @HaskellCore plonk
        witness         = (PlonkupWitnessInput witnessInputs, ps)
        (input, proof)  = prove @(PlonkN 1 2) @HaskellCore setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

stateCheckVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1 -> F -> (SetupBytes, InputBytes, ProofBytes)
stateCheckVerificationBytes x ps state =
    let F n             = state
        state'          = toZp n :: Fr
        (omega, k1, k2) = getParams 2
        witnessInputs   = unsafeToVector [state']
        plonk           = Plonk omega k1 k2 emptyCircuit x :: PlonkN 1 2
        setupP          = setupProve @_ @HaskellCore plonk
        setupV          = setupVerify @_ @HaskellCore plonk
        witness         = (PlonkupWitnessInput witnessInputs, ps)
        (input, proof)  = prove @(PlonkN 1 2) @HaskellCore setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

stateCheckVerificationBytes' :: Fr -> PlonkupProverSecret BLS12_381_G1 -> F -> (SetupBytes, InputBytes, ProofBytes)
stateCheckVerificationBytes' x ps state =
    let F n             = state
        state'          = toZp n :: Fr
        (omega, k1, k2) = getParams 2
        witnessInputs   = unsafeToVector [negate state']
        plonk           = Plonk omega k1 k2 emptyCircuit x :: PlonkN 1 2
        setupP          = setupProve @_ @HaskellCore plonk
        setupV          = setupVerify @_ @HaskellCore plonk
        witness         = (PlonkupWitnessInput witnessInputs, ps)
        (input, proof)  = prove @(PlonkN 1 2) @HaskellCore setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

testEmptyCircuit :: Fr -> PlonkupProverSecret BLS12_381_G1 -> Haskell.Bool
testEmptyCircuit x ps =
    let (s, i, p) = emptyCircuitVerificationBytes x ps
    in verify @PlonkPlutus @HaskellCore s i p
