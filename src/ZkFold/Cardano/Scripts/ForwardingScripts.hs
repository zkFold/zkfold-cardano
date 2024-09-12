module ZkFold.Cardano.Scripts.ForwardingScripts where

import           PlutusLedgerApi.V3
import           PlutusTx.AssocMap                  (keys)
import           PlutusTx.Prelude                   (Bool (..), find, isJust, ($), BuiltinUnit, Maybe (..), check, (.), error)

import           ZkFold.Cardano.Plonk.OnChain.Data  (FMLabel)
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
forwardingMint :: FMLabel -> BuiltinByteString -> () -> ScriptContext -> Bool
forwardingMint _label symbolHash _ ctx =
    -- Searching for the minting script with a specific hash
    isJust $ find (eqCurrencySymbol sc) reds
    where
        -- Constructing the minting script purpose
        sc = CurrencySymbol symbolHash

        -- Finding scripts to be executed
        reds = keys $ txInfoRedeemers $ scriptContextTxInfo ctx

untypedForwardingReward :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
untypedForwardingReward datum redeemer ctx =
  check
    ( forwardingReward
        (unsafeFromBuiltinData datum)
        (unsafeFromBuiltinData redeemer)
        (unsafeFromBuiltinData ctx)
    )

untypedForwardingMint :: FMLabel -> BuiltinData -> BuiltinUnit
untypedForwardingMint label' ctx' =
  let ctx = unsafeFromBuiltinData ctx' in
    case scriptContextScriptInfo ctx of
      SpendingScript _ (Just dat) -> check $
        forwardingMint
          label'
          (unsafeFromBuiltinData . getDatum $ dat)
          (unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx)
          ctx
      _                             -> error ()