module Main where

import           Prelude
import           Tests.Plonk (specPlonk)
import           Tests.Plonkup (specPlonkup)

main :: IO ()
main = do
    -- specPlonk
    specPlonkup
