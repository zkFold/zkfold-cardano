{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module ZkFold.Cardano.UPLC (symbolicVerifierCompiled, plonkVerifierCompiled, verifyPlonkCompiled, forwardingRewardCompiled, forwardingMintCompiled) where

import           Flat.Types                               ()
import           PlutusLedgerApi.V3
import           PlutusTx                                 (CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                         (BuiltinUnit)
import           Prelude                                  hiding (Bool, Eq (..), Fractional (..), Num (..), length, ($),
                                                           (.))

import           ZkFold.Cardano.Plonk                     (untypedVerifyPlonk)
import           ZkFold.Cardano.Plonk.OnChain.Data        (SetupBytes)
import           ZkFold.Cardano.Scripts.ForwardingScripts (untypedForwardingMint, untypedForwardingReward)
import           ZkFold.Cardano.Scripts.PlonkVerifier     (untypedPlonkVerifier)
import           ZkFold.Cardano.Scripts.SymbolicVerifier  (untypedSymbolicVerifier)


symbolicVerifierCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
symbolicVerifierCompiled contract =
    $$(compile [|| untypedSymbolicVerifier ||])
    `unsafeApplyCode` liftCodeDef contract

plonkVerifierCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
plonkVerifierCompiled computation =
    $$(compile [|| untypedPlonkVerifier ||])
    `unsafeApplyCode` liftCodeDef computation

verifyPlonkCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
verifyPlonkCompiled computation =
    $$(compile [|| untypedVerifyPlonk ||])
    `unsafeApplyCode` liftCodeDef computation

forwardingRewardCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
forwardingRewardCompiled =
    $$(compile [|| untypedForwardingReward ||])

forwardingMintCompiled :: Integer -> CompiledCode (BuiltinData -> BuiltinUnit)
forwardingMintCompiled label =
    $$(compile [|| untypedForwardingMint ||])
    `unsafeApplyCode` liftCodeDef label
