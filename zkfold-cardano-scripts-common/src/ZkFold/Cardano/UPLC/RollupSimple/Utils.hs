module ZkFold.Cardano.UPLC.RollupSimple.Utils (
  fillWithZeros,
  fillWithZeros2,
  fillWithZeros3WithAdd,
  addressToBS,
  byteStringToInteger',
  toSymbolicValue,
) where

import           GHC.ByteOrder            (ByteOrder (..))
import           PlutusLedgerApi.V1.Value (flattenValue)
import           PlutusLedgerApi.V3
import           PlutusTx.Prelude

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications -XDataKinds
>>> import           PlutusLedgerApi.V1.Value (flattenValue)
>>> import           PlutusLedgerApi.V3
>>> import           PlutusTx.Prelude
-}

{- |
>>> fillWithZeros 3
[0,0,0]
-}
{-# INLINEABLE fillWithZeros #-}
fillWithZeros :: Integer -> [Integer]
fillWithZeros n
  | n < 0 = traceError "fillWithZeros: negative index"
  | otherwise = replicate n 0

{- | Fill with zeros for 2 dimensions.

>>> fillWithZeros2 2 3 []
[0,0,0,0,0,0]
-}
{-# INLINEABLE fillWithZeros2 #-}
fillWithZeros2 :: Integer -> Integer -> [Integer] -> [Integer]
fillWithZeros2 nout nin acc
  | nout < 0 = traceError "fillWithZeros2: negative index"
  | nout == 0 = acc
  | otherwise = fillWithZeros2 (nout - 1) nin $ fillWithZeros nin <> acc

{- | Fill with zeros for 3 dimensions with a catch that for each element in outer dimension, we always add `0` element to serialized inner matrix of 2 dimensions.

@fillWithZeros3WithAdd 2 3 4 []@ should have @2 + (2 * 3 * 4) = 26@ zeros.

>>> fillWithZeros3WithAdd 2 2 3 []
[0,0,0,0,0,0,0,0,0,0,0,0,0,0]
-}
{-# INLINEABLE fillWithZeros3WithAdd #-}
fillWithZeros3WithAdd :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
fillWithZeros3WithAdd n1 n2 n3 acc
  | n1 < 0 = traceError "fillWithZeros3WithAdd: negative index"
  | n1 == 0 = acc
  | otherwise = fillWithZeros3WithAdd (n1 - 1) n2 n3 $ fillWithZeros2 n2 n3 (0 : acc)

-- | Simply concatenates payment and staking credential.
{-# INLINEABLE addressToBS #-}
addressToBS :: Address -> BuiltinByteString
addressToBS Address {..} =
  credentialToBS addressCredential
    <> maybe
      mempty
      ( \case
          StakingHash cred -> credentialToBS cred
          StakingPtr {} -> traceError "addressToBS: StakingPtr not supported"
      )
      addressStakingCredential
 where
  credentialToBS :: Credential -> BuiltinByteString
  credentialToBS = \case
    PubKeyCredential pkh -> getPubKeyHash pkh
    ScriptCredential sh -> getScriptHash sh

{-# INLINEABLE byteStringToInteger' #-}
byteStringToInteger' :: BuiltinByteString -> Integer
byteStringToInteger' = byteStringToInteger BigEndian

{- | Convert 'Value' to our symbolic ledger representation.

>>> toSymbolicValue 3 mempty
[0,0,0,0,0,0,0,0,0]

>>> toSymbolicValue 3 (singleton adaSymbol adaToken 1)
[0,0,1,0,0,0,0,0,0]
-}
{-# INLINEABLE toSymbolicValue #-}
toSymbolicValue ::
  -- | Maximum number of assets that can be present in the layer-2 output.
  Integer ->
  Value ->
  [Integer]
toSymbolicValue maxAssets v =
  let v' = flattenValue v
   in foldMap (\(cs, tn, amt) -> [byteStringToInteger' $ unCurrencySymbol cs, byteStringToInteger' $ unTokenName tn, amt]) v' <> fillWithZeros2 (maxAssets - length v') 3 []

