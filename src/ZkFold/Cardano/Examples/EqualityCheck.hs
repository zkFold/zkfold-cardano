module ZkFold.Cardano.Examples.EqualityCheck where

import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import qualified Prelude                                     as Haskell

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Data.Vector                     (Vector (..), unsafeToVector)
import           ZkFold.Base.Protocol.NonInteractiveProof    (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonkup                (Plonkup (..))
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams)
import           ZkFold.Base.Protocol.Plonkup.Witness        (PlonkupWitnessInput (..))
import           ZkFold.Cardano.OffChain.Plonkup             (PlonkupN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.OnChain.Plonkup              (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data         (InputBytes, ProofBytes, SetupBytes)
import           ZkFold.Symbolic.Class                       (Symbolic (..))
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compileForceOne)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement           (FieldElement)

equalityCheckContract :: forall a c . (Symbolic c, FromConstant a (BaseField c)) => a -> FieldElement c -> Bool c
equalityCheckContract targetValue inputValue = inputValue == fromConstant targetValue

equalityCheckVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1 -> Fr -> (SetupBytes, InputBytes, ProofBytes)
equalityCheckVerificationBytes x ps targetValue =
    let ac = compileForceOne @Fr (equalityCheckContract @Fr @(ArithmeticCircuit Fr (Vector 1)) targetValue) :: ArithmeticCircuit Fr (Vector 1) (Vector 1)

        (omega, k1, k2) = getParams 32
        witnessInputs  = unsafeToVector [targetValue]
        plonkup   = Plonkup omega k1 k2 ac x :: PlonkupN 1 32
        setupP  = setupProve @_ @HaskellCore plonkup
        setupV  = setupVerify @_ @HaskellCore plonkup
        witness = (PlonkupWitnessInput witnessInputs, ps)
        (input, proof) = prove @(PlonkupN 1 32) @HaskellCore setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

testEqualityCheckContract :: Fr -> PlonkupProverSecret BLS12_381_G1 -> Fr -> Haskell.Bool
testEqualityCheckContract x ps targetValue =
    let (s, i, p) = equalityCheckVerificationBytes x ps targetValue
    in verify @PlonkupPlutus @HaskellCore s i p
