{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ZkFold.Cardano.UPLC.RollupData where

import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V3
import           PlutusLedgerApi.V3.Contexts              (ownCurrencySymbol)
import           PlutusTx                                 (makeIsDataIndexed)
import           PlutusTx.AssocMap                        (toList, lookup)
import           PlutusTx.Prelude                         hiding ((*), (+), toList)
import           Prelude                                  (Show)

import           ZkFold.Cardano.OnChain.Utils             (dataToBlake)

data RollupDataRedeemer =
      NewData [BuiltinByteString]
    | OldData
    -- ^ Adjust the stake in the rollup.
  deriving stock (Show, Generic)

makeIsDataIndexed ''RollupDataRedeemer [('NewData,0),('OldData,1)]

-- | Plutus script for verifying a ZkFold Rollup state transition.
{-# INLINABLE rollupData #-}
rollupData :: RollupDataRedeemer -> ScriptContext -> Bool
rollupData (NewData update) ctx =
  let
    -- Get the current rollup output
    symbol = ownCurrencySymbol ctx

    minted = map (unTokenName . fst) $ toList $ case lookup symbol $ getValue $ txInfoMint $ scriptContextTxInfo ctx of
      Just v -> v
      Nothing -> traceError "rollupData: no minted value"
  in
    -- Check that the token name is the hash of the data
    minted == [dataToBlake update]
rollupData OldData ctx =
  let
    -- Get the current rollup output
    symbol = ownCurrencySymbol ctx

    burned = toList $ case lookup symbol $ getValue $ txInfoMint $ scriptContextTxInfo ctx of
      Just v -> v
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
