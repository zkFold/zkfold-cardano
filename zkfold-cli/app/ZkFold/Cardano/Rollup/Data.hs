module ZkFold.Cardano.Rollup.Data where

import           PlutusLedgerApi.V1.Value     (Lovelace (..))
import           PlutusLedgerApi.V3           (DatumHash (..))
import           PlutusTx.Builtins            (BuiltinByteString, ByteOrder (..), blake2b_256, integerToByteString)
import           Prelude                      (IO, Int, Integer, Monad (..), ($))
import           System.Random                (randomRIO)

import           ZkFold.Cardano.OnChain.Utils (dataToBlake)


rollupFee :: Lovelace
rollupFee = Lovelace 15000000

threadLovelace :: Lovelace
threadLovelace = Lovelace 3000000

updateLength :: Int
updateLength = 3

rmax :: Integer
rmax = 1000

minReq :: Lovelace
minReq = Lovelace 995610

evolve :: [BuiltinByteString] -> IO [BuiltinByteString]
evolve bs = do
  n <- randomRIO (1, rmax)
  return $ dataToBlake n : bs

--  Datum hash example
datumHashBSEx1 :: BuiltinByteString
datumHashBSEx1 = blake2b_256 $ integerToByteString BigEndian 0 43

datumHashEx1 :: DatumHash
datumHashEx1 = DatumHash datumHashBSEx1
