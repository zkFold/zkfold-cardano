module Main where

import           Prelude
import           Tests.Compatibility (specCompatibility)
import           Test.QuickCheck

import           ZkFold.Cardano.Examples.EmptyCircuit

main :: IO ()
main = do
    x <- generate arbitrary
    ps <- generate arbitrary
    print emptyCircuit
    print $ testEmptyCircuit x ps
    specCompatibility
