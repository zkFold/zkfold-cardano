module ZkFold.Cardano.Examples.EqualityCheck where

import           Data.ByteString                             (ByteString)
import           Data.Map                                    (fromList)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import qualified Prelude                                     as Haskell

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, BLS12_381_G2, Fr)
import qualified ZkFold.Base.Data.Vector                     as V
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..), PlonkProverSecret, PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                        (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OffChain               (mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.Plonk.OnChain.Data           (InputBytes, ProofBytes, SetupBytes)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))

type PlonkBase32 = Plonk 32 1 BLS12_381_G1 BLS12_381_G2 ByteString

equalityCheckContract :: forall b a a' . (FromConstant a' (b 1 a), Eq (Bool (b 1 a)) (b 1 a)) => a' -> b 1 a -> Bool (b 1 a)
equalityCheckContract targetValue inputValue = inputValue == fromConstant targetValue

equalityCheckVerificationBytes :: Fr -> PlonkProverSecret BLS12_381_G1 -> Fr -> (SetupBytes, InputBytes, ProofBytes)
equalityCheckVerificationBytes x ps targetValue =
    let Bool ac = compile @Fr (equalityCheckContract @ArithmeticCircuit @Fr targetValue) :: Bool (ArithmeticCircuit 1 Fr)

        (omega, k1, k2) = getParams 5
        witnessInputs  = fromList [(1, targetValue), (V.item $ acOutput ac, 1)]
        indexTargetValue = V.Vector [1]
        plonk   = Plonk @32 omega k1 k2 indexTargetValue ac x
        setupP  = setupProve @PlonkBase32 plonk
        setupV  = setupVerify @PlonkBase32 plonk
        witness = (PlonkWitnessInput witnessInputs, ps)
        (input, proof) = prove @PlonkBase32 setupP witness

    in (mkSetup setupV, mkInput input, mkProof @32 (mkSetup setupV) proof)

testEqualityCheckContract :: Fr -> PlonkProverSecret BLS12_381_G1 -> Fr -> Haskell.Bool
testEqualityCheckContract x ps targetValue =
    let (s, i, p) = equalityCheckVerificationBytes x ps targetValue
    in verify @PlonkPlutus s i p
