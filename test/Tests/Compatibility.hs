{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Tests.Compatibility (specCompatibility) where

import           Data.Functor.Classes                     (Show1 (..))
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

instance Show1 U1 where
  liftShowsPrec _ _ = showsPrec

instance Show1 (U1 :*: U1) where
  liftShowsPrec _ _ = showsPrec

instance Show1 Par1 where
  liftShowsPrec _ _ = undefined

instance Show1 (Par1 :*: U1) where
  liftShowsPrec _ _ = undefined

instance Arbitrary (U1 a) where
  arbitrary = return U1
instance Arbitrary1 U1 where
  liftArbitrary _ = return U1

instance Arbitrary1 (U1 :*: U1) where
  liftArbitrary _ = return (U1 :*: U1)

-- Arbitrary1 (Par1 :*: U1)

instance Arbitrary Void where
  arbitrary = undefined

instance Arbitrary1 (Par1 :*: U1) where
  liftArbitrary _ = return (Par1 undefined :*: U1)

specCompatibility :: IO ()
specCompatibility = hspec $ do
    describe "Plonkup verifier compatibility test" $ do
        it "should pass" $ property $ propCompatibility @(PlonkupN (Par1 :*: U1) 32) @PlonkupPlutus
