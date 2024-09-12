{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module ZkFold.Cardano.UPLC (symbolicVerifierCompiled, plonkVerifierCompiled, verifyPlonkCompiled) where

import           Flat.Types                              ()
import           PlutusLedgerApi.V3
import           PlutusTx                                (CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                        (BuiltinUnit)
import           Prelude                                 hiding (Bool, Eq (..), Fractional (..), Num (..), length, ($),
                                                          (.))

import           ZkFold.Cardano.Plonk                    (untypedVerifyPlonk)
import           ZkFold.Cardano.Plonk.OnChain.Data       (SetupBytes)
import           ZkFold.Cardano.Scripts.PlonkVerifier    (untypedPlonkVerifier)
import           ZkFold.Cardano.Scripts.SymbolicVerifier (untypedSymbolicVerifier)


symbolicVerifierCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
symbolicVerifierCompiled contract =
    $$(compile [|| untypedSymbolicVerifier ||])
    `unsafeApplyCode` liftCodeDef contract

plonkVerifierCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
plonkVerifierCompiled computation =
    $$(compile [|| untypedPlonkVerifier ||])
    `unsafeApplyCode` liftCodeDef computation

verifyPlonkCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
verifyPlonkCompiled computation =
    $$(compile [|| untypedVerifyPlonk ||])
    `unsafeApplyCode` liftCodeDef computation
