{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts (compiledSymbolicVerifier, compiledPlonkVerifier, compiledPlonkVerify) where

import           PlutusLedgerApi.V3                       (ScriptContext, BuiltinData)
import           PlutusTx                                 (CompiledCode, UnsafeFromData (..))
import           PlutusTx.Prelude                         (Bool, check, ($))
import           PlutusTx.TH                              (compile)

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.ScriptsVerifier

compiledSymbolicVerifier :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledSymbolicVerifier = $$(compile [|| untypedSymbolicVerifier ||])
  where
    untypedSymbolicVerifier :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    untypedSymbolicVerifier datum redeemer ctx =
      check
        ( symbolicVerifier
            (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )

compiledPlonkVerifier :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPlonkVerifier = $$(compile [|| untypedPlonkVerifier ||])
  where
    untypedPlonkVerifier :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    untypedPlonkVerifier datum redeemer ctx =
      check
        ( plonkVerifier
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
