module Rollup.Example where

import           PlutusLedgerApi.V3
import           PlutusTx.Builtins  (ByteOrder (..))
import           PlutusTx.Prelude

--  Datum hash example
datumHashBSEx1 :: BuiltinByteString
datumHashBSEx1 = blake2b_256 $ integerToByteString BigEndian 0 43

datumHashEx1 :: DatumHash
datumHashEx1 = DatumHash datumHashBSEx1
