module Tests.Verifier (specVerifier) where

import           Data.ByteString                             (ByteString)
import           Data.Map                                    (fromList, keys)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import qualified Prelude                                     as Haskell
import           Test.Hspec                                  (describe, hspec, it)
import           Test.QuickCheck                             (Testable (property))

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Data.Vector                     (Vector(..))
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..), PlonkProverSecret, PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                        (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OffChain               (mkInput, mkProof, mkSetup)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile, applyArgs)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Types                       (Symbolic)

type PlonkBase32 = Plonk 32 1 ByteString

lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => a' -> a -> Bool a
lockedByTxId targetValue inputValue = inputValue == fromConstant targetValue

testVerifier :: Fr -> PlonkProverSecret -> Fr -> Haskell.Bool
testVerifier x ps targetValue =
    let Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) targetValue)
        ac' = applyArgs ac [targetValue]

        (omega, k1, k2) = getParams 5
        inputs  = fromList [(acOutput ac', 1)]
        plonk   = Plonk omega k1 k2 (Vector @1 $ keys inputs) ac' x :: PlonkBase32
        sP      = setupProve plonk
        sV      = setupVerify plonk
        w       = (PlonkWitnessInput inputs, ps)
        (input, proof) = prove @PlonkBase32 sP w

    in verify @PlonkPlutus (mkSetup sV) (mkInput input) (mkProof @32 (mkSetup sV) proof)

specVerifier :: IO ()
specVerifier = hspec $ do
    describe "Verifier test (LockedByTxId contract)" $ do
        it "should pass" $ property testVerifier
