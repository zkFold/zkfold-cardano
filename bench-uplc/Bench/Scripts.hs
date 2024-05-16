{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts (compiledSymbolicVerifier, compiledPlonkVerifier, compiledPlonkVerify) where

import           PlutusLedgerApi.V3                       (BuiltinData)
import           PlutusTx                                 (CompiledCode, UnsafeFromData (..))
import           PlutusTx.Prelude                         (Bool, check, ($))
import           PlutusTx.TH                              (compile)

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.ScriptsVerifier           (RedeemerVerifier (..), plonkVerifier, symbolicVerifier)

compiledSymbolicVerifier :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledSymbolicVerifier = $$(compile [|| untypedSymbolicVerifier ||])
  where
    untypedSymbolicVerifier :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    untypedSymbolicVerifier _ redeemer ctx =
      check
        ( symbolicVerifier
            -- (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )

compiledPlonkVerifier :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPlonkVerifier = $$(compile [|| untypedPlonkVerifier ||])
  where
    untypedPlonkVerifier :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    untypedPlonkVerifier _ redeemer ctx =
      check
        ( plonkVerifier
            -- (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )

compiledPlonkVerify :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPlonkVerify =
  $$(compile [|| \_ r _ -> check $ apply (unsafeFromBuiltinData r) ||])
  where
    apply :: RedeemerVerifier -> Bool
    apply redeemer = verify @PlonkPlutus s i p
      where (RedeemerVerifier s i p) = redeemer
