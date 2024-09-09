{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts (symbolicVerifierCompiled, plonkVerifierCompiled, verifyPlonkCompiled) where

import           Flat.Types                               ()
import           PlutusLedgerApi.V3
import           PlutusTx                                 (CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                         (BuiltinUnit, check, ($), (.))
import           Prelude                                  hiding (Bool, Eq (..), Fractional (..), Num (..), length, ($),
                                                           (.))

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..), verify)
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain.Data        (SetupBytes)
import           ZkFold.Cardano.Scripts.PlonkVerifier     (plonkVerifier)
import           ZkFold.Cardano.Scripts.SymbolicVerifier  (symbolicVerifier)


symbolicVerifierCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
symbolicVerifierCompiled contract =
    $$(compile [|| untypedSymbolicVerifier ||])
    `unsafeApplyCode` liftCodeDef contract
  where
    untypedSymbolicVerifier :: SetupBytes -> BuiltinData -> BuiltinUnit
    untypedSymbolicVerifier contract' ctx' =
      let ctx = unsafeFromBuiltinData ctx' in
      check $
        symbolicVerifier
            contract'
            (unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx)
            ctx

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

verifyPlonkCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
verifyPlonkCompiled computation =
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
