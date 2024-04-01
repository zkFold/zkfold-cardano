{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

module Tests.Verifier (specVerifier) where

import           Data.Map                                    (fromList)
import           Prelude                                     hiding (Num(..), Bool, Fractional(..), Eq(..), length)
import qualified Prelude                                     as Haskell
import           Test.Hspec
import           Test.QuickCheck

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Protocol.ARK.Plonk              (WitnessInputPlonk (..), Plonk (..), ProverSecretPlonk(..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof(..))
import           ZkFold.Cardano.Plonk                        (PlonkBBS, PlonkPlutus, mkSetup, mkInput, mkProof)
import           ZkFold.Symbolic.Cardano.Types.Tx            (TxId(..))
import           ZkFold.Symbolic.Compiler                    (compile, ArithmeticCircuit(..))
import           ZkFold.Symbolic.Data.Bool                   (Bool(..))
import           ZkFold.Symbolic.Data.Eq                     (Eq(..))
import           ZkFold.Symbolic.Types                       (Symbolic)

lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => TxId a' -> TxId a -> () -> Bool a
lockedByTxId (TxId targetId) (TxId txId) _ = txId == fromConstant targetId

testVerifier :: Fr -> ProverSecretPlonk -> Fr -> Haskell.Bool
testVerifier x ps targetId =
    let Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) (TxId targetId)) :: Bool (ArithmeticCircuit Fr)

        (omega, k1, k2) = getParams 5
        inputs  = fromList [(1, targetId), (acOutput ac, 1)]
        plonk   = Plonk omega k1 k2 inputs ac x
        s       = setup @PlonkBBS plonk
        w       = (WitnessInputPlonk inputs, ps)
        (input, proof) = prove @PlonkBBS s w

    in verify @PlonkPlutus (mkSetup s) (mkInput input) (mkProof proof)

specVerifier :: IO ()
specVerifier = hspec $ do
    describe "Verifier test (LockedByTxId contract)" $ do
        it "should pass" $ property testVerifier