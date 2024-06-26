{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.ForwardingScripts where

import           PlutusLedgerApi.V3                       (ScriptContext (..), TxInfo (..), BuiltinByteString, ScriptPurpose (..), ScriptHash (..), Credential (..))
import           PlutusTx.AssocMap (lookup)
import           PlutusTx.Prelude                         (Bool (..), ($), isJust)

-- | The Plutus spending script that forwards verification to a rewarding script.
{-# INLINABLE forwardingSpendingScript #-}
forwardingSpendingScript :: BuiltinByteString -> () -> () -> ScriptContext -> Bool
forwardingSpendingScript contractHash _ _ ctx =
    -- Searching for the rewarding script with a specific hash
    isJust $ lookup purp reds
    where
        info  = scriptContextTxInfo ctx

        -- Constructing the rewarding script purpose
        purp = Rewarding $ ScriptCredential $ ScriptHash contractHash

        -- Finding scripts to be executed
        reds = txInfoRedeemers info