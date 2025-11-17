{-# LANGUAGE NamedFieldPuns #-}
module ZkFold.Cardano.OnChain.Utils where

import           PlutusLedgerApi.V3 (BuiltinByteString, Credential, CurrencySymbol, ScriptPurpose (..), ToData (..),
                                     TxInInfo (..), TxOutRef)
import           PlutusTx.Builtins  (Integer, blake2b_224, serialiseData)
import           PlutusTx.Prelude   (Bool (..), Eq (..), Maybe, find, (.))

type ScriptLabel = Integer -- Implements distinct addresses for scripts

-- | hash transaction data with blake2b_224
{-# INLINEABLE dataToBlake #-}
dataToBlake :: (ToData a) => a -> BuiltinByteString
dataToBlake = blake2b_224 . serialiseData . toBuiltinData

-- https://github.com/IntersectMBO/plutus/issues/6273
{-# INLINEABLE eqRewardingPurpose #-}
eqRewardingPurpose :: Credential -> ScriptPurpose -> Bool
eqRewardingPurpose a (Rewarding b) = a == b
eqRewardingPurpose _ _             = False

-- https://github.com/IntersectMBO/plutus/issues/6273
{-# INLINEABLE eqMintingPurpose #-}
eqMintingPurpose :: CurrencySymbol -> ScriptPurpose -> Bool
eqMintingPurpose a (Minting b) = a == b
eqMintingPurpose _ _           = False

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: [TxInInfo] -> TxOutRef -> Maybe TxInInfo
findOwnInput' txInfoInputs txOutRef = find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
