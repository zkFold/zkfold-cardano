module ZkFold.Cardano.Examples.EqualityCheck where

import           Data.Aeson                             (FromJSON, ToJSON)
import           GHC.Generics                           (Generic, Par1 (..), U1 (..), type (:*:) (..))
import           Prelude                                hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import qualified Prelude                                as Haskell

import           ZkFold.Algebra.Class                   (FromConstant (..))
import           ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, Fr)
import           ZkFold.Cardano.OffChain.Plonkup        (PlonkupN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.OnChain.Plonkup         (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data    (InputBytes, ProofBytes, SetupBytes)
import           ZkFold.Data.HFunctor                   (hmap)
import           ZkFold.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Protocol.Plonkup                (Plonkup (..))
import           ZkFold.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)
import           ZkFold.Protocol.Plonkup.Utils          (getParams, getSecrectParams)
import           ZkFold.Protocol.Plonkup.Witness        (PlonkupWitnessInput (..))
import           ZkFold.Symbolic.Class                  (Symbolic (..))
import           ZkFold.Symbolic.Compiler               (ArithmeticCircuit (..), compileWith)
import           ZkFold.Symbolic.Data.Bool              (Bool (..))
import           ZkFold.Symbolic.Data.Eq                (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement      (FieldElement)

data EqualityCheckContract = EqualityCheckContract {
    x           :: Fr
  , ps          :: PlonkupProverSecret BLS12_381_G1_Point
  , targetValue :: Fr
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

equalityCheckContract :: forall a c . (Symbolic c, FromConstant a (BaseField c)) => a -> FieldElement c -> Bool c
equalityCheckContract targetValue inputValue = inputValue == fromConstant targetValue

equalityCheckVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1_Point -> Fr -> (SetupBytes, InputBytes, ProofBytes)
equalityCheckVerificationBytes x ps targetValue =
    let ac = hmap (:*: U1) $ compileWith @Fr id (\(Par1 i) -> (U1 :*: U1, Par1 i :*: U1)) (equalityCheckContract @Fr targetValue) :: ArithmeticCircuit Fr Par1 (Par1 :*: U1)

        (omega, k1, k2) = getParams 32
        witnessInputs   = Par1 targetValue
        (gs, h1) = getSecrectParams x
        plonkup = Plonkup omega k1 k2 ac h1 gs :: PlonkupN Par1 Par1 U1 32
        setupP  = setupProve plonkup
        setupV  = setupVerify plonkup
        witness = (PlonkupWitnessInput witnessInputs, ps)
        (input, proof) = prove @(PlonkupN Par1 Par1 U1 32) setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

testEqualityCheckContract :: Fr -> PlonkupProverSecret BLS12_381_G1_Point -> Fr -> Haskell.Bool
testEqualityCheckContract x ps targetValue =
    let (s, i, p) = equalityCheckVerificationBytes x ps targetValue
    in verify @PlonkupPlutus s i p
