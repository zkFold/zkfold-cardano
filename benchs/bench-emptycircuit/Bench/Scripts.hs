{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts where

import           PlutusLedgerApi.V3                (BuiltinData, getRedeemer, scriptContextRedeemer)
import           PlutusTx                          (CompiledCode, liftCodeDef, unsafeApplyCode, unsafeFromBuiltinData)
import           PlutusTx.Prelude                  (Bool (..), BuiltinUnit, Integer, check, ($), (.))
import           PlutusTx.TH                       (compile)

import           ZkFold.Cardano.Plonk.OnChain.Data (SetupBytes)
import           ZkFold.Cardano.Benchs.PubInput    (pubInput, symbolicVerifierBench1)


compiledAlwaysSucceeds :: Integer -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledAlwaysSucceeds n =
    $$(compile [|| untypedAlwaysSucceeds ||])
    `unsafeApplyCode` liftCodeDef n
  where
    untypedAlwaysSucceeds :: Integer -> BuiltinData -> BuiltinUnit
    untypedAlwaysSucceeds _ _ = check True

{-
compiledSymbolicVerifier :: SetupBytes -> CompiledCode (BuiltinData  -> BuiltinUnit)
compiledSymbolicVerifier computation =
    $$(compile [|| untypedSymbolicVerifier ||])
    `unsafeApplyCode` liftCodeDef computation
  where
    untypedSymbolicVerifier :: SetupBytes -> BuiltinData -> BuiltinData -> BuiltinUnit
    untypedSymbolicVerifier computation' redeemer ctx =
      check
        ( symbolicVerifier
            computation'
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )




compiledSymbolicVerifierBench1 :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledSymbolicVerifierBench1 computation =
    $$(compile [|| untypedSymbolicVerifierBench1 ||])
    `unsafeApplyCode` liftCodeDef computation
  where
    untypedSymbolicVerifierBench1 :: SetupBytes -> BuiltinData -> BuiltinUnit
    untypedSymbolicVerifierBench1 computation' ctx' =
      let ctx = unsafeFromBuiltinData ctx' in
        check $
          symbolicVerifierBench1
            computation'
            (unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx)
            ctx
-}
