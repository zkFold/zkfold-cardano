{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Scripts (forwardingMintCompiled, plonkVerifierCompiled) where

import           PlutusLedgerApi.V3                       (BuiltinData, ScriptContext (..), ScriptInfo (..), getDatum,
                                                           getRedeemer)
import           PlutusTx                                 (CompiledCode, UnsafeFromData (..), liftCodeDef,
                                                           unsafeApplyCode)
import           PlutusTx.Prelude                         (BuiltinUnit, Integer, Maybe (..), check, error, ($), (.))
import           PlutusTx.TH                              (compile)

import           ZkFold.Cardano.Plonk.OnChain.Data        (FMLabel, SetupBytes)
import           ZkFold.Cardano.Scripts.ForwardingScripts (forwardingMint)
import           ZkFold.Cardano.Scripts.PlonkVerifier     (plonkVerifier)

forwardingMintCompiled :: Integer -> CompiledCode (BuiltinData -> BuiltinUnit)
forwardingMintCompiled label =
    $$(compile [|| untypedforwardingMint ||])
    `unsafeApplyCode` liftCodeDef label
  where
    untypedforwardingMint :: FMLabel -> BuiltinData -> BuiltinUnit
    untypedforwardingMint label' ctx' =
      let ctx = unsafeFromBuiltinData ctx' in
        case scriptContextScriptInfo ctx of
          SpendingScript _ (Just dat) -> check $
            forwardingMint
              label'
              (unsafeFromBuiltinData . getDatum $ dat)
              (unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx)
              ctx
          _                             -> error ()

plonkVerifierCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
plonkVerifierCompiled computation =
    $$(compile [|| untypedPlonkVerifier ||])
    `unsafeApplyCode` liftCodeDef computation
  where
    untypedPlonkVerifier :: SetupBytes -> BuiltinData -> BuiltinUnit
    untypedPlonkVerifier computation' ctx' =
      let ctx = unsafeFromBuiltinData ctx' in
        check $
          plonkVerifier
            computation'
            (unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx)
            ctx
