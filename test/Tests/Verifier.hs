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
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk, ParamsPlonk (..), WitnessInputPlonk (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof(..))
import           ZkFold.Symbolic.Arithmetization             (ArithmeticCircuit (..))
import           ZkFold.Symbolic.Compiler                    (compile)
import           ZkFold.Symbolic.Data.Bool                   (Bool(..))
import           ZkFold.Symbolic.Data.Eq                     (Eq(..))
import           ZkFold.Cardano.Types.Tx                     (TxId(..))
import           ZkFold.Symbolic.Types                       (Symbolic)
import           ZkFold.Symbolic.Verifier                    (PlonkPlutus, mkSetup, mkInput)

lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => TxId a' -> TxId a -> () -> Bool a
lockedByTxId (TxId targetId) (TxId txId) _ = txId == fromConstant targetId

testVerifier :: SetupSecret Plonk -> ProverSecret Plonk -> Fr -> Haskell.Bool
testVerifier x ps targetId =
    let Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) (TxId targetId)) :: Bool (ArithmeticCircuit Fr)

        (omega, k1, k2) = getParams 5
        inputs  = fromList [(1, targetId), (acOutput ac, 1)]
        pp      = ParamsPlonk omega k1 k2 inputs ac
        s       = setup @Plonk pp x
        w       = WitnessInputPlonk inputs
        (input, proof) = prove @Plonk ps s w

    in verify @PlonkPlutus (mkSetup s) (mkInput input) proof

specVerifier :: IO ()
specVerifier = hspec $ do
    describe "Verifier test (LockedByTxId contract)" $ do
        it "should pass" $ property testVerifier