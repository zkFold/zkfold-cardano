{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts (compiledSymbolicVerifier, compiledPlonkVerifier, compiledPlonkVerify) where

import           PlutusLedgerApi.V3                       (ScriptContext, BuiltinData)
import           PlutusTx                                 (CompiledCode, unsafeApplyCode, liftCodeDef, UnsafeFromData (..))
import           PlutusTx.Prelude                         (Bool, check, ($))
import           PlutusTx.TH                              (compile)

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (ParamsVerifier, RedeemerVerifier (..), DatumVerifier)
import           ZkFold.Cardano.ScriptsVerifier           (plonkVerifier, symbolicVerifier)

compiledSymbolicVerifier :: ParamsVerifier -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledSymbolicVerifier params' = $$(compile [|| untypedSymbolicVerifier ||]) `unsafeApplyCode` liftCodeDef params'
  where
    untypedSymbolicVerifier :: ParamsVerifier -> BuiltinData -> BuiltinData -> BuiltinData -> ()
    untypedSymbolicVerifier params datum redeemer ctx =
      check
        ( symbolicVerifier
            params
            (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )

compiledPlonkVerifier :: ParamsVerifier -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPlonkVerifier params' = $$(compile [|| untypedPlonkVerifier ||]) `unsafeApplyCode` liftCodeDef params'
  where
    untypedPlonkVerifier :: ParamsVerifier -> BuiltinData -> BuiltinData -> BuiltinData -> ()
    untypedPlonkVerifier params datum redeemer ctx =
      check
        ( plonkVerifier
            params
            (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )

compiledPlonkVerify :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPlonkVerify =
  $$(compile [|| \d r s -> check $ apply (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData s) ||])
  where
    apply :: DatumVerifier -> RedeemerVerifier -> ScriptContext -> Bool
    apply _ redeemer _ = verify @PlonkPlutus s i p
      where (RedeemerVerifier s i p) = redeemer
