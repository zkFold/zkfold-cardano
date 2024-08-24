{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Scripts (compiledforwardingMint, compiledPlonkVerifier) where

import           PlutusLedgerApi.V3                       (BuiltinData, ScriptContext(..), ScriptInfo(..), getDatum, getRedeemer)
import           PlutusTx                                 (CompiledCode, UnsafeFromData (..), liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                         (check, error, ($), (.), BuiltinUnit, Maybe(..), Integer)
import           PlutusTx.TH                              (compile)

import           ZkFold.Cardano.Plonk.OnChain.Data        (FMLabel, SetupBytes)
import           ZkFold.Cardano.Scripts.PlonkVerifier     (plonkVerifier)
import           ZkFold.Cardano.Scripts.ForwardingScripts (forwardingMint)

compiledforwardingMint :: Integer -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledforwardingMint label =
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

compiledPlonkVerifier :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledPlonkVerifier computation =
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
