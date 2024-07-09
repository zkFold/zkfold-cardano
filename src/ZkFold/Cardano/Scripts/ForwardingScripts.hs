{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.Scripts.ForwardingScripts where

import           PlutusLedgerApi.V3
import           PlutusTx.AssocMap                  (keys)
import           PlutusTx.Prelude                   (Bool (..), find, isJust, ($))

import           ZkFold.Cardano.Plonk.OnChain.Utils (eqCredential, eqCurrencySymbol)

-- | The Plutus spending script that forwards verification to a rewarding script.
{-# INLINABLE forwardingReward #-}
forwardingReward :: BuiltinByteString -> () -> ScriptContext -> Bool
forwardingReward contractHash _ ctx =
    -- Searching for the rewarding script with a specific hash
    isJust $ find (eqCredential sc) reds
    where
        -- Constructing the rewarding script purpose
        sc = ScriptCredential $ ScriptHash contractHash

        -- Finding scripts to be executed
        reds = keys $ txInfoRedeemers $ scriptContextTxInfo ctx

-- | The Plutus spending script that forwards verification to a minting script.
{-# INLINABLE forwardingMint #-}
forwardingMint :: BuiltinByteString -> () -> ScriptContext -> Bool
forwardingMint symbolHash _ ctx =
    -- Searching for the minting script with a specific hash
    isJust $ find (eqCurrencySymbol sc) reds
    where
        -- Constructing the minting script purpose
        sc = CurrencySymbol symbolHash

        -- Finding scripts to be executed
        reds = keys $ txInfoRedeemers $ scriptContextTxInfo ctx
