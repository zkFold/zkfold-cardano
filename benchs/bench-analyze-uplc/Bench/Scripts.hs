{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts (compiledSymbolicVerifier, compiledPlonkVerifier, compiledVerifyPlonk) where

import           PlutusLedgerApi.V3                      (BuiltinData)
import           PlutusTx                                (CompiledCode, UnsafeFromData (..), liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                        (check, BuiltinUnit)
import           PlutusTx.TH                             (compile)

import           ZkFold.Cardano.Plonk.OnChain.Data       (SetupBytes)
import           ZkFold.Cardano.Scripts.PlonkVerifier    (plonkVerifier)
import           ZkFold.Cardano.Scripts.SymbolicVerifier (symbolicVerifier)
import ZkFold.Cardano.Plonk
import ZkFold.Base.Protocol.NonInteractiveProof (verify)

compiledSymbolicVerifier :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
compiledSymbolicVerifier contract =
    $$(compile [|| untypedSymbolicVerifier ||])
    `unsafeApplyCode` liftCodeDef contract
  where
    untypedSymbolicVerifier :: SetupBytes -> BuiltinData -> BuiltinData -> BuiltinUnit
    untypedSymbolicVerifier contract' redeemer ctx =
      check
        ( symbolicVerifier
            contract'
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )

compiledPlonkVerifier :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
compiledPlonkVerifier computation =
    $$(compile [|| untypedPlonkVerifier ||])
    `unsafeApplyCode` liftCodeDef computation
  where
    untypedPlonkVerifier :: SetupBytes -> BuiltinData -> BuiltinData -> BuiltinUnit
    untypedPlonkVerifier computation' redeemer ctx =
      check
        ( plonkVerifier
            computation'
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )

compiledVerifyPlonk :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
compiledVerifyPlonk computation =
    $$(compile [|| untypedVerifyPlonk ||])
    `unsafeApplyCode` liftCodeDef computation
  where
    untypedVerifyPlonk :: SetupBytes -> BuiltinData -> BuiltinData -> BuiltinUnit
    untypedVerifyPlonk computation' input proof =
      check
        ( verify @PlonkPlutus
            computation'
            (unsafeFromBuiltinData input)
            (unsafeFromBuiltinData proof)
        )
