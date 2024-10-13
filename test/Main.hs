module Main where

import           Prelude
import           Tests.Compatibility (specCompatibility)

main :: IO ()
main = do
    specCompatibility
