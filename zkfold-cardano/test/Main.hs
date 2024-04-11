module Main where

import           Prelude
import           Tests.Data     (verifyIsSatisfied)
import           Tests.Verifier (specVerifier)

main :: IO ()
main = do
    -- verifyIsSatisfied
    specVerifier
