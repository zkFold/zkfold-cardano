{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts (parkingSpotCompiled) where

import           PlutusLedgerApi.V3 (BuiltinData, ScriptContext, unsafeFromBuiltinData)
import           PlutusTx           (CompiledCode, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude   (Bool (..), BuiltinUnit, Integer, check, ($))
import           PlutusTx.TH        (compile)


------- TYPED SCRIPTS ------

{-# INLINABLE parkingSpot #-}
parkingSpot :: Integer -> ScriptContext -> Bool
parkingSpot _ _ = True

------ UNTYPED SCRIPTS -----

{-# INLINABLE untypedParkingSpot #-}
untypedParkingSpot :: Integer -> BuiltinData -> BuiltinUnit
untypedParkingSpot tag ctx' =
  let
    ctx = unsafeFromBuiltinData ctx'
  in
    check $ parkingSpot tag ctx

----- COMPILED SCRIPTS -----

parkingSpotCompiled :: Integer -> CompiledCode (BuiltinData -> BuiltinUnit)
parkingSpotCompiled tag =
    $$(compile [|| untypedParkingSpot ||])
    `unsafeApplyCode` liftCodeDef tag
