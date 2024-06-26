module Tests.Verifier (specVerifier) where

import           Prelude                               hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           Test.Hspec                            (describe, hspec, it)
import           Test.QuickCheck                       (Testable (property))

import           ZkFold.Cardano.Examples.EqualityCheck (testEqualityCheckContract)

specVerifier :: IO ()
specVerifier = hspec $ do
    describe "Verifier test (Equality Check contract)" $ do
        it "should pass" $ property testEqualityCheckContract
