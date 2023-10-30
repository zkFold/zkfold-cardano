module Main where

import           Prelude

import           Examples.Fibonacci   (exampleFibonacci)
import           Examples.MiMCHash    (exampleMiMC)
import           Examples.ReverseList (exampleReverseList)

main :: IO ()
main = do
    exampleFibonacci
    exampleMiMC
    exampleReverseList