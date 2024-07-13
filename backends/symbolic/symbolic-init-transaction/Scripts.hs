{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Scripts (compiledForwardingReward, compiledSymbolicVerifier) where

import           PlutusLedgerApi.V3                       (BuiltinData)
import           PlutusTx                                 (CompiledCode, UnsafeFromData (..), liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                         (check)
import           PlutusTx.TH                              (compile)

import           ZkFold.Cardano.Plonk.OnChain.Data        (SetupBytes)
import           ZkFold.Cardano.Scripts.ForwardingScripts (forwardingReward)
import           ZkFold.Cardano.Scripts.SymbolicVerifier  (symbolicVerifier)

compiledForwardingReward :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledForwardingReward =
    $$(compile [|| untypedForwardingReward ||])
  where
    untypedForwardingReward :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    untypedForwardingReward datum redeemer ctx =
      check
        ( forwardingReward
            (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )

compiledSymbolicVerifier :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> ())
compiledSymbolicVerifier computation =
    $$(compile [|| untypedSymbolicVerifier ||])
    `unsafeApplyCode` liftCodeDef computation
  where
    untypedSymbolicVerifier :: SetupBytes -> BuiltinData -> BuiltinData -> ()
    untypedSymbolicVerifier computation' redeemer ctx =
      check
        ( symbolicVerifier
            computation'
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )
