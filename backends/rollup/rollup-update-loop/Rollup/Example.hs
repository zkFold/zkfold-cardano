module Rollup.Example where

import           PlutusLedgerApi.V3
import           PlutusTx.Builtins            (ByteOrder (..))
import           PlutusTx.Prelude
import           Prelude                      (IO)
import           System.Random                (randomRIO)

import           ZkFold.Cardano.OnChain.Utils (dataToBlake)

rmax :: Integer
rmax = 1000

evolve :: [BuiltinByteString] -> IO [BuiltinByteString]
evolve bs = do
  n <- randomRIO (1, rmax)
  return $ dataToBlake n : bs

--  Datum hash example
datumHashBSEx1 :: BuiltinByteString
datumHashBSEx1 = blake2b_256 $ integerToByteString BigEndian 0 43

datumHashEx1 :: DatumHash
datumHashEx1 = DatumHash datumHashBSEx1
