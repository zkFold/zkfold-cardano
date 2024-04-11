module Main where

import           Prelude
import           System.IO                            (stdout)

import           ZkFoldBenchmark.Verifier.RunVerifier (runVerifier)

main :: IO ()
main = do runVerifier stdout
