module Tests.Plonkup (specPlonkup) where

import           Prelude                                  hiding (Eq (..), Fractional (..), Num (..), length)
import           Test.Hspec                               (describe, hspec, it)
import           Test.QuickCheck                          (Testable (property))

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProofTestData (..),
                                                           nipCompatibility)
import           ZkFold.Cardano.OffChain.Plonkup          (PlonkN)
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)

propCompatibility :: NonInteractiveProofTestData (PlonkN 1 32) HaskellCore -> Bool
propCompatibility (TestData a w) = nipCompatibility @(PlonkN 1 32) @PlonkupPlutus @HaskellCore a w

specPlonkup :: IO ()
specPlonkup = hspec $ do
    describe "Plonkup verifier compatibility test" $ do
        it "should pass" $ property propCompatibility

