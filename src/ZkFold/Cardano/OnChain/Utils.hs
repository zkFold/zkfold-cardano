module ZkFold.Cardano.OnChain.Utils where

import           PlutusLedgerApi.V3
import           PlutusTx.Builtins
import           PlutusTx.Prelude   (Bool (..), Eq (..), (.))

type ScriptLabel = Integer  -- Implements distinct addresses for scripts

-- | hash transaction data with blake2b_224
{-# INLINABLE dataToBlake #-}
dataToBlake :: ToData a => a -> BuiltinByteString
dataToBlake = blake2b_224 . serialiseData . toBuiltinData

-- https://github.com/IntersectMBO/plutus/issues/6273
{-# INLINABLE eqRewardingPurpose #-}
eqRewardingPurpose :: Credential -> ScriptPurpose -> Bool
eqRewardingPurpose a (Rewarding b) = a == b
eqRewardingPurpose _ _             = False

-- https://github.com/IntersectMBO/plutus/issues/6273
{-# INLINABLE eqMintingPurpose #-}
eqMintingPurpose :: CurrencySymbol -> ScriptPurpose -> Bool
eqMintingPurpose a (Minting b) = a == b
eqMintingPurpose _ _           = False
