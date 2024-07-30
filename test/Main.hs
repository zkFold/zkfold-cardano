module Main where

import           Prelude
import           Tests.Verifier (specVerifier)
import Tests.Primer ( hprop_plutus_v3 )
import Hedgehog (check)

main :: IO Bool
main = do
    specVerifier
    check hprop_plutus_v3
