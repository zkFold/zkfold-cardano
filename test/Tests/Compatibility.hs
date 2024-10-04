module Tests.Compatibility (specCompatibility) where

import           Prelude                                  hiding (Eq (..), Fractional (..), Num (..), length)
import           Test.Hspec                               (describe, hspec, it)
import           Test.QuickCheck                          (Testable (property))

import           ZkFold.Base.Protocol.NonInteractiveProof (CompatibleNonInteractiveProofs, HaskellCore,
                                                           NonInteractiveProofTestData (..), nipCompatibility)
import           ZkFold.Cardano.OffChain.Plonk            (PlonkN)
import           ZkFold.Cardano.OffChain.Plonkup          (PlonkupN)
import           ZkFold.Cardano.OnChain.Plonk             (PlonkPlutus)
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)

propCompatibility :: forall a b . CompatibleNonInteractiveProofs a b HaskellCore =>
    NonInteractiveProofTestData a HaskellCore -> Bool
propCompatibility (TestData a w) = nipCompatibility @a @b @HaskellCore a w

specCompatibility :: IO ()
specCompatibility = hspec $ do
    describe "Plonk verifier compatibility test" $ do
        it "should pass" $ property $ propCompatibility @(PlonkN 1 32) @PlonkPlutus
    describe "Plonkup verifier compatibility test" $ do
        it "should pass" $ property $ propCompatibility @(PlonkupN 1 32) @PlonkupPlutus
