{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.Scripts.ForwardingScripts where

import           PlutusLedgerApi.V3
import           PlutusTx.AssocMap  (lookup)
import           PlutusTx.Prelude   (Bool (..), isJust, ($))

-- | The Plutus spending script that forwards verification to a rewarding script.
{-# INLINABLE forwardingReward #-}
forwardingReward :: BuiltinByteString -> () -> ScriptContext -> Bool
forwardingReward contractHash _ ctx =
    -- Searching for the rewarding script with a specific hash
    isJust $ lookup purp reds
    where
        -- Constructing the rewarding script purpose
        purp = Rewarding $ ScriptCredential $ ScriptHash contractHash

        -- Finding scripts to be executed
        reds = txInfoRedeemers $ scriptContextTxInfo ctx

-- | The Plutus spending script that forwards verification to a minting script.
{-# INLINABLE forwardingMint #-}
forwardingMint :: BuiltinByteString -> () -> ScriptContext -> Bool
forwardingMint symbolHash _ ctx =
    -- Searching for the minting script with a specific hash
    isJust $ lookup purp reds
    where
        -- Constructing the minting script purpose
        purp = Minting $ CurrencySymbol symbolHash

        -- Finding scripts to be executed
        reds = txInfoRedeemers $ scriptContextTxInfo ctx
