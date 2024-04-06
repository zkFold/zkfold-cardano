module Main where

import ZkFoldBenchmark.Verifier.RunVerifier (runVerifier)
import System.IO (stdout)

main :: IO ()
main = do runVerifier stdout
