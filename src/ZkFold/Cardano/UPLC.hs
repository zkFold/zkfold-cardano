{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC
  ( symbolicVerifierCompiled
  , plonkVerifierCompiled
  , verifyPlonkCompiled
  , forwardingRewardCompiled
  , forwardingMintCompiled
  , rollupCompiled
  , rollupCompiled'
  , parkingSpotCompiled
  ) where

import           Flat.Types                            ()
import           PlutusLedgerApi.V3
import           PlutusTx                              (CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                      (BuiltinUnit)
import           Prelude                               hiding (Bool, Eq (..), Fractional (..), Num (..), length, ($),
                                                        (.))

import           ZkFold.Cardano.OnChain.Plonk          (untypedVerifyPlonk)
import           ZkFold.Cardano.OnChain.Plonk.Data     (SetupBytes)
import           ZkFold.Cardano.UPLC.ForwardingScripts (untypedForwardingMint, untypedForwardingReward)
import           ZkFold.Cardano.UPLC.PlonkVerifier     (untypedPlonkVerifier)
import           ZkFold.Cardano.UPLC.Rollup            (untypedParkingSpot, untypedRollup, untypedRollup')
import           ZkFold.Cardano.UPLC.SymbolicVerifier  (untypedSymbolicVerifier)


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

parkingSpotCompiled :: Integer -> CompiledCode (BuiltinData -> BuiltinUnit)
parkingSpotCompiled tag =
    $$(compile [|| untypedParkingSpot ||])
    `unsafeApplyCode` liftCodeDef tag

rollupCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
rollupCompiled computation =
    $$(compile [|| untypedRollup ||])
    `unsafeApplyCode` liftCodeDef computation

rollupCompiled' :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
rollupCompiled' computation =
    $$(compile [|| untypedRollup' ||])
    `unsafeApplyCode` liftCodeDef computation
