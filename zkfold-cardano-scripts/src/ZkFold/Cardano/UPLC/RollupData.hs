{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.RollupData where

import           GHC.Generics                 (Generic)
import           PlutusLedgerApi.V3           (Redeemer (..), ScriptContext (..), TokenName (..), TxInfo (..),
                                               UnsafeFromData (..), mintValueToMap)
import           PlutusLedgerApi.V3.Contexts  (ownCurrencySymbol)
import           PlutusTx                     (CompiledCode, compile, makeIsDataIndexed)
import           PlutusTx.AssocMap            (lookup, toList)
import           PlutusTx.Prelude             hiding (toList, (*), (+))
import           Prelude                      (Show)

import           ZkFold.Cardano.OnChain.Utils (dataToBlake)

data RollupDataRedeemer =
      NewData [BuiltinByteString]
    | OldData
    -- ^ Adjust the stake in the rollup.
  deriving stock (Show, Generic)

makeIsDataIndexed ''RollupDataRedeemer [('NewData,0),('OldData,1)]

-- | Plutus script for posting rollup data on-chain.
-- Creates a token with the hash of the data as the name.
{-# INLINABLE rollupData #-}
rollupData :: RollupDataRedeemer -> ScriptContext -> Bool
rollupData (NewData update) ctx =
  let
    -- Get the current rollup output
    symbol = ownCurrencySymbol ctx

    minted = map (unTokenName . fst) $ toList $ case lookup symbol $ mintValueToMap $ txInfoMint $ scriptContextTxInfo ctx of
      Just v  -> v
      Nothing -> traceError "rollupData: no minted value"
  in
    -- Check that the token name is the hash of the data
    minted == [dataToBlake update]
rollupData OldData ctx =
  let
    -- Get the current rollup output
    symbol = ownCurrencySymbol ctx

    burned = toList $ case lookup symbol $ mintValueToMap $ txInfoMint $ scriptContextTxInfo ctx of
      Just v  -> v
      Nothing -> traceError "rollupData: no burned value"
  in
    -- Check that all tokens are burned
    all (\(_, n) -> n < 0) burned

{-# INLINABLE untypedRollupData #-}
untypedRollupData :: BuiltinData -> BuiltinUnit
untypedRollupData ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ rollupData redeemer ctx

rollupDataCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
rollupDataCompiled =
    $$(compile [|| untypedRollupData ||])
