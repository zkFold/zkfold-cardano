module Rollup.Example where

import           PlutusLedgerApi.V3
import           Prelude                      (IO, Integer, return, ($))
import           System.Random                (randomRIO)

import           ZkFold.Cardano.OnChain.Utils (dataToBlake)

rmax :: Integer
rmax = 1000

evolve :: [BuiltinByteString] -> IO [BuiltinByteString]
evolve bs = do
  n <- randomRIO (1, rmax)
  return $ dataToBlake n : bs
