{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Compatibility (specCompatibility) where

import           GHC.Generics
import           Prelude                             hiding (Eq (..), Fractional (..), Num (..), length)
import           Test.Hspec                          (describe, hspec, it)
import           Test.QuickCheck                     (Testable (property))
import           Test.QuickCheck.Arbitrary

import           ZkFold.Cardano.OffChain.Plonkup     (PlonkupN)
import           ZkFold.Cardano.OnChain.Plonkup      (PlonkupPlutus)
import           ZkFold.Protocol.NonInteractiveProof (CompatibleNonInteractiveProofs, NonInteractiveProof (..),
                                                      nipCompatibility)

propCompatibility :: forall a b . CompatibleNonInteractiveProofs a b =>
    (a, Witness a) -> Bool
propCompatibility (a, w) = nipCompatibility @a @b a w

instance Arbitrary1 Par1 where
  liftArbitrary = fmap Par1

specCompatibility :: IO ()
specCompatibility = hspec $ do
    describe "Plonkup verifier compatibility test" $ do
        it "should pass" $ property $ propCompatibility @(PlonkupN Par1 Par1 32) @PlonkupPlutus
