{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Scripts (compiledforwardingMint, compiledPlonkVerifier) where

import           PlutusLedgerApi.V3                      (BuiltinData)
import           PlutusTx                                (CompiledCode, UnsafeFromData (..), liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                        (check)
import           PlutusTx.TH                             (compile)

import           ZkFold.Cardano.Plonk.OnChain.Data       (SetupBytes)
import           ZkFold.Cardano.Scripts.PlonkVerifier    (plonkVerifier)
import           ZkFold.Cardano.Scripts.ForwardingScripts (forwardingMint)

compiledforwardingMint :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledforwardingMint =
    $$(compile [|| untypedforwardingMint ||])
  where
    untypedforwardingMint :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    untypedforwardingMint datum redeemer ctx =
      check
        ( forwardingMint
            (unsafeFromBuiltinData datum)
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )

compiledPlonkVerifier :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> ())
compiledPlonkVerifier computation =
    $$(compile [|| untypedPlonkVerifier ||])
    `unsafeApplyCode` liftCodeDef computation
  where
    untypedPlonkVerifier :: SetupBytes -> BuiltinData -> BuiltinData -> ()
    untypedPlonkVerifier computation' redeemer ctx =
      check
        ( plonkVerifier
            computation'
            (unsafeFromBuiltinData redeemer)
            (unsafeFromBuiltinData ctx)
        )
