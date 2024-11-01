{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ZkFold.Cardano.UPLC.RollupDeposit where

import           GHC.Generics                 (Generic)
import           PlutusLedgerApi.V3
import           PlutusTx                     (makeIsDataIndexed)
import           PlutusTx.Prelude             hiding (toList, (*), (+))
import           Prelude                      (Show)

import           ZkFold.Cardano.OnChain.Utils (dataToBlake)

data RollupDepositRedeemer = RollupDepositRedeemer Integer BuiltinByteString
  deriving stock (Show, Generic)

makeIsDataIndexed ''RollupDepositRedeemer [('RollupDepositRedeemer,0)]

-- | Plutus proxy script for depositing funds into the rollup.
-- This is an example script. There could be other ways to implement this.
{-# INLINABLE rollupDeposit #-}
rollupDeposit :: BuiltinByteString -> RollupDepositRedeemer -> ScriptContext -> Bool
rollupDeposit vk (RollupDepositRedeemer i sig) ctx =
  let
    -- Get the i-th output
    out = txInfoOutputs (scriptContextTxInfo ctx) !! i
  in
    verifyEd25519Signature vk (dataToBlake out) sig

{-# INLINABLE untypedRollupDeposit #-}
untypedRollupDeposit :: BuiltinByteString -> BuiltinData -> BuiltinUnit
untypedRollupDeposit vk ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ rollupDeposit vk redeemer ctx
