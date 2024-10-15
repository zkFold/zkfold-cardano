module ZkFold.Cardano.Examples.EmptyCircuit where

import           GHC.Generics                                (Par1(..))
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import qualified Prelude                                     as Haskell

import           ZkFold.Base.Algebra.Basic.Class             (zero)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Data.HFunctor                   (hmap)
import           ZkFold.Base.Data.Vector                     (Vector (..), unsafeToVector, item, singleton)
import           ZkFold.Base.Protocol.NonInteractiveProof    (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonk
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams)
import           ZkFold.Base.Protocol.Plonkup.Witness
import           ZkFold.Cardano.OffChain.Plonk               (PlonkN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.OnChain.Plonk                (PlonkPlutus)
import           ZkFold.Cardano.OnChain.Plonk.Data           (InputBytes, ProofBytes, SetupBytes)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), eval)
import           ZkFold.Symbolic.Data.FieldElement

emptyCircuit :: ArithmeticCircuit Fr (Vector 1) (Vector 1)
emptyCircuit =
    let FieldElement ac = zero
    in hmap (\(Par1 x) -> singleton x) ac

emptyCircuitVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1 -> (SetupBytes, InputBytes, ProofBytes)
emptyCircuitVerificationBytes x ps =
    let (omega, k1, k2) = getParams 2
        witnessInputs  = unsafeToVector [item $ eval emptyCircuit $ singleton zero]
        plonk   = Plonk omega k1 k2 emptyCircuit x :: PlonkN 1 2
        setupP  = setupProve @_ @HaskellCore plonk
        setupV  = setupVerify @_ @HaskellCore plonk
        witness = (PlonkupWitnessInput witnessInputs, ps)
        (input, proof) = prove @(PlonkN 1 2) @HaskellCore setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

testEmptyCircuit :: Fr -> PlonkupProverSecret BLS12_381_G1 -> Haskell.Bool
testEmptyCircuit x ps =
    let (s, i, p) = emptyCircuitVerificationBytes x ps
    in verify @PlonkPlutus @HaskellCore s i p
