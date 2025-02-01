module Rollup.Data where

import           PlutusLedgerApi.V1.Value (Lovelace (..))
import           Prelude                  (Int, Integer)

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
