{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts where

import           PlutusLedgerApi.V3                (BuiltinData)
import           PlutusTx                          (CompiledCode, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                  (Bool (..), BuiltinUnit, Integer, check)
import           PlutusTx.TH                       (compile)


compiledAlwaysSucceeds :: Integer -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledAlwaysSucceeds n =
    $$(compile [|| untypedAlwaysSucceeds ||])
    `unsafeApplyCode` liftCodeDef n
  where
    untypedAlwaysSucceeds :: Integer -> BuiltinData -> BuiltinUnit
    untypedAlwaysSucceeds _ _ = check True
