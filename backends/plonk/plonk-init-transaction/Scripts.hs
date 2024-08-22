{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Scripts (compiledforwardingMint, compiledPlonkVerifier) where

import           PlutusLedgerApi.V3                       (BuiltinData, ScriptContext(..), ScriptInfo(..), getDatum, getRedeemer)
import           PlutusTx                                 (CompiledCode, UnsafeFromData (..), liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                         (check, error, ($), (.), BuiltinUnit, Maybe(..))
import           PlutusTx.TH                              (compile)

import           ZkFold.Cardano.Plonk.OnChain.Data        (SetupBytes)
import           ZkFold.Cardano.Scripts.PlonkVerifier     (plonkVerifier)
import           ZkFold.Cardano.Scripts.ForwardingScripts (forwardingMint)

compiledforwardingMint :: CompiledCode (BuiltinData -> BuiltinUnit)
compiledforwardingMint =
    $$(compile [|| untypedforwardingMint ||])
  where
    untypedforwardingMint :: BuiltinData -> BuiltinUnit
    untypedforwardingMint ctx' =
      let ctx = unsafeFromBuiltinData ctx' in
        case scriptContextScriptInfo ctx of
          SpendingScript _ (Just dat) -> check $
            forwardingMint
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
