{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts where

import           PlutusLedgerApi.V3                (BuiltinData, getRedeemer, scriptContextRedeemer)
import           PlutusTx                          (CompiledCode, liftCodeDef, unsafeApplyCode, unsafeFromBuiltinData)
import           PlutusTx.Prelude                  (Bool (..), BuiltinUnit, check, not, ($), (.))
import           PlutusTx.TH                       (compile)

import           ZkFold.Cardano.Plonk.OnChain.Data (InputBytes, SetupBytes)
import           ZkFold.Cardano.Benchs.PubInput    (pubInput, symbolicVerifierBench1)


compiledPubInput :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledPubInput =
    $$(compile [|| untypedPubInput ||])
  where
    untypedPubInput :: BuiltinData -> BuiltinUnit
    untypedPubInput = check . pubInput . unsafeFromBuiltinData

compiledAlwaysSucceeds :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledAlwaysSucceeds =
    $$(compile [|| untypedAlwaysSucceeds ||])
  where
    untypedAlwaysSucceeds :: BuiltinData -> BuiltinUnit
    untypedAlwaysSucceeds _ = check True

compiledSymbolicVerifierBench1 :: SetupBytes -> InputBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledSymbolicVerifierBench1 computation input =
    $$(compile [|| untypedSymbolicVerifierBench1 ||])
    `unsafeApplyCode` liftCodeDef computation
    `unsafeApplyCode` liftCodeDef input
  where
    untypedSymbolicVerifierBench1 :: SetupBytes -> InputBytes -> BuiltinData -> BuiltinUnit
    untypedSymbolicVerifierBench1 computation' input' ctx' =
      let ctx = unsafeFromBuiltinData ctx' in
        check . not $
          symbolicVerifierBench1
            computation'
            input'
            (unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx)
            ctx

