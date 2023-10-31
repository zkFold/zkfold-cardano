module Main where

import           Prelude
import           System.Directory       (createDirectoryIfMissing)

import           Examples.Fibonacci     (exampleFibonacci)
import           Examples.LEQ           (exampleLEQ)
import           Examples.MiMCHash      (exampleMiMC)
import           Examples.ReverseList   (exampleReverseList)
import           Examples.VestingScript (exampleVesting)

main :: IO ()
main = do
    createDirectoryIfMissing True "compiled_scripts"

    exampleLEQ
    exampleFibonacci
    exampleMiMC
    exampleReverseList
    exampleVesting