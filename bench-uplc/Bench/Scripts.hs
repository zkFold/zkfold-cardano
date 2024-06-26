{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts (compiledSymbolicVerifier, compiledPlonkVerifier, compiledPlonkVerify) where

import           PlutusLedgerApi.V3                       (BuiltinData)
import           PlutusTx                                 (CompiledCode, UnsafeFromData (..), unsafeApplyCode, liftCodeDef, toBuiltinData)
import           PlutusTx.Prelude                         (Bool, check, ($))
import           PlutusTx.TH                              (compile)

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (SetupBytes, InputBytes, ProofBytes)
import           ZkFold.Cardano.PlonkVerifier             (plonkVerifier)
import           ZkFold.Cardano.SymbolicVerifier          (symbolicVerifier)

compiledSymbolicVerifier :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> ())
compiledSymbolicVerifier contract =
    $$(compile [|| untypedSymbolicVerifier ||])
    `unsafeApplyCode` liftCodeDef (toBuiltinData contract)
  where
    untypedSymbolicVerifier :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    untypedSymbolicVerifier par redeemer ctx =
      check
        ( symbolicVerifier
            (unsafeFromBuiltinData par)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )

compiledPlonkVerifier :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> ())
compiledPlonkVerifier computation =
    $$(compile [|| untypedPlonkVerifier ||])
    `unsafeApplyCode` liftCodeDef (toBuiltinData computation)
  where
    untypedPlonkVerifier :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    untypedPlonkVerifier par redeemer ctx =
      check
        ( plonkVerifier
            (unsafeFromBuiltinData par)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )

compiledPlonkVerify :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPlonkVerify =
  $$(compile [|| \_ r _ -> check $ apply (unsafeFromBuiltinData r) ||])
  where
    apply :: (SetupBytes, InputBytes, ProofBytes) -> Bool
    apply (s, i, p) = verify @PlonkPlutus s i p
