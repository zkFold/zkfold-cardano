module ZkFold.Cardano.UPLC.ForwardingScripts where

import           PlutusLedgerApi.V3
import           PlutusTx.AssocMap            (keys)
import           PlutusTx.Prelude             (Bool (..), BuiltinUnit, Maybe (..), check, error, find, isJust, ($), (.))

import           ZkFold.Cardano.OnChain.Utils (ScriptLabel, eqMintingPurpose, eqRewardingPurpose)

-- | The Plutus spending script that forwards verification to a rewarding script.
{-# INLINABLE forwardingReward #-}
forwardingReward :: BuiltinByteString -> () -> ScriptContext -> Bool
forwardingReward contractHash _ ctx =
    -- Searching for the rewarding script with a specific hash
    isJust $ find (eqRewardingPurpose sc) reds
    where
        -- Constructing the rewarding script purpose
        sc = ScriptCredential $ ScriptHash contractHash

        -- Finding scripts to be executed
        reds = keys $ txInfoRedeemers $ scriptContextTxInfo ctx

-- | The Plutus spending script that forwards verification to a minting script.
{-# INLINABLE forwardingMint #-}
forwardingMint :: ScriptLabel -> BuiltinByteString -> () -> ScriptContext -> Bool
forwardingMint _label symbolHash _ ctx =
    -- Searching for the minting script with a specific hash
    isJust $ find (eqMintingPurpose sc) reds
    where
        -- Constructing the minting script purpose
        sc = CurrencySymbol symbolHash

        -- Finding scripts to be executed
        reds = keys $ txInfoRedeemers $ scriptContextTxInfo ctx

{-# INLINABLE untypedForwardingReward #-}
untypedForwardingReward :: BuiltinData -> BuiltinUnit
untypedForwardingReward ctx' =
  let ctx = unsafeFromBuiltinData ctx' in
    case scriptContextScriptInfo ctx of
      SpendingScript _ (Just dat) -> check $
        forwardingReward
          (unsafeFromBuiltinData . getDatum $ dat)
          (unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx)
          ctx
      _                           -> error ()

{-# INLINABLE untypedForwardingMint #-}
untypedForwardingMint :: ScriptLabel -> BuiltinData -> BuiltinUnit
untypedForwardingMint label' ctx' =
  let ctx = unsafeFromBuiltinData ctx' in
    case scriptContextScriptInfo ctx of
      SpendingScript _ (Just dat) -> check $
        forwardingMint
          label'
          (unsafeFromBuiltinData . getDatum $ dat)
          (unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx)
          ctx
      _                           -> error ()
