module Tests.Verifier (specVerifier) where

import           Prelude                                  hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           Test.Hspec                               (describe, hspec, it)
import           Test.QuickCheck                          (Testable (property))

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProofTestData (..), nipCompatibility)
import           ZkFold.Cardano.Plonk.OffChain            (PlonkN)
import           ZkFold.Cardano.Plonk                     (PlonkPlutus(..))

specVerifier :: IO ()
specVerifier = hspec $ do
    describe "Plonk verifier compatibility test" $ do
        it "should pass" $ property (\(TestData a w) -> nipCompatibility @(PlonkN 32) @PlonkPlutus a w)

