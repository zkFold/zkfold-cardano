module Tests.Verifier (specVerifier) where

import           Prelude                                  hiding (Eq (..), Fractional (..), Num (..), length)
import           Test.Hspec                               (describe, hspec, it)
import           Test.QuickCheck                          (Testable (property))

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProofTestData (..), HaskellCore, nipCompatibility)
import           ZkFold.Cardano.OffChain.Plonk            (PlonkN)
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)

propCompatibility :: NonInteractiveProofTestData (PlonkN 1 32) HaskellCore -> Bool
propCompatibility (TestData a w) = nipCompatibility @(PlonkN 1 32) @PlonkPlutus @HaskellCore a w

specVerifier :: IO ()
specVerifier = hspec $ do
    describe "Plonk verifier compatibility test" $ do
        it "should pass" $ property propCompatibility

