{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Compatibility (specCompatibility) where

import           GHC.Base                                 (Void)
import           GHC.Generics
import           Prelude                                  hiding (Eq (..), Fractional (..), Num (..), length)
import           Test.Hspec                               (describe, hspec, it)
import           Test.QuickCheck                          (Testable (property))
import           Test.QuickCheck.Arbitrary

import           ZkFold.Base.Protocol.NonInteractiveProof (CompatibleNonInteractiveProofs, HaskellCore,
                                                           NonInteractiveProof (..), nipCompatibility)
import           ZkFold.Cardano.OffChain.Plonkup          (PlonkupN)
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)

propCompatibility :: forall a b . CompatibleNonInteractiveProofs a b HaskellCore =>
    (a, Witness a) -> Bool
propCompatibility (a, w) = nipCompatibility @a @b @HaskellCore a w

instance Arbitrary (U1 a) where
  arbitrary = return U1

instance Arbitrary1 U1 where
  liftArbitrary _ = return U1

instance Arbitrary1 (U1 :*: U1) where
  liftArbitrary _ = return (U1 :*: U1)

instance Arbitrary Void where
  arbitrary = return $ error "Uninhabited data type"

instance Arbitrary1 (Par1 :*: U1) where
  liftArbitrary = fmap (flip (:*:) U1 . Par1)

specCompatibility :: IO ()
specCompatibility = hspec $ do
    describe "Plonkup verifier compatibility test" $ do
        it "should pass" $ property $ propCompatibility @(PlonkupN (U1 :*: U1) (Par1 :*: U1) 32) @PlonkupPlutus
