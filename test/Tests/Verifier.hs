module Tests.Verifier (specVerifier) where

import           Data.Map                                    (fromList)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import qualified Prelude                                     as Haskell
import           Test.Hspec                                  (describe, hspec, it)
import           Test.QuickCheck                             (Testable (property))

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..), PlonkProverSecret, PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                        (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OffChain               (Plonk32, mkInput, mkProof, mkSetup)
import           ZkFold.Symbolic.Cardano.Types.Tx            (TxId (..))
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Types                       (Symbolic)

lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => TxId a' -> TxId a -> () -> Bool a
lockedByTxId (TxId targetId) (TxId txId) _ = txId == fromConstant targetId

testVerifier :: Fr -> PlonkProverSecret -> Fr -> Haskell.Bool
testVerifier x ps targetId =
    let Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) (TxId targetId))

        (omega, k1, k2) = getParams 5
        inputs  = fromList [(1, targetId), (acOutput ac, 1)]
        plonk   = Plonk omega k1 k2 inputs ac x
        s       = setup @Plonk32 plonk
        w       = (PlonkWitnessInput inputs, ps)
        (input, proof) = prove @Plonk32 s w

    in verify @PlonkPlutus (mkSetup s) (mkInput input) (mkProof (mkSetup s) proof)

specVerifier :: IO ()
specVerifier = hspec $ do
    describe "Verifier test (LockedByTxId contract)" $ do
        it "should pass" $ property testVerifier
