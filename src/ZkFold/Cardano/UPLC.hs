{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC
  ( plonkVerifierTxCompiled
  , plonkVerifierTokenCompiled
  , plonkVerifierCompiled
  , forwardingRewardCompiled
  , forwardingMintCompiled
  , rollupCompiled
  , parkingSpotCompiled
  ) where

import           Flat.Types                             ()
import           PlutusLedgerApi.V3
import           PlutusTx                               (CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                       (BuiltinUnit)
import           Prelude                                hiding (Bool, Eq (..), Fractional (..), Num (..), length, ($),
                                                         (.))

import           ZkFold.Cardano.OnChain.Plonkup         (untypedPlonkVerifier)
import           ZkFold.Cardano.OnChain.Plonkup.Data    (SetupBytes)
import           ZkFold.Cardano.UPLC.ForwardingScripts  (untypedForwardingMint, untypedForwardingReward)
import           ZkFold.Cardano.UPLC.PlonkVerifierToken (untypedPlonkVerifierToken)
import           ZkFold.Cardano.UPLC.PlonkVerifierTx    (untypedPlonkVerifierTx)
import           ZkFold.Cardano.UPLC.Rollup             (RollupSetup, untypedParkingSpot, untypedRollup)


plonkVerifierTxCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
plonkVerifierTxCompiled contract =
    $$(compile [|| untypedPlonkVerifierTx ||])
    `unsafeApplyCode` liftCodeDef contract

plonkVerifierTokenCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
plonkVerifierTokenCompiled computation =
    $$(compile [|| untypedPlonkVerifierToken ||])
    `unsafeApplyCode` liftCodeDef computation

plonkVerifierCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
plonkVerifierCompiled computation =
    $$(compile [|| untypedPlonkVerifier ||])
    `unsafeApplyCode` liftCodeDef computation

forwardingRewardCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
forwardingRewardCompiled =
    $$(compile [|| untypedForwardingReward ||])

forwardingMintCompiled :: Integer -> CompiledCode (BuiltinData -> BuiltinUnit)
forwardingMintCompiled label =
    $$(compile [|| untypedForwardingMint ||])
    `unsafeApplyCode` liftCodeDef label

rollupCompiled :: RollupSetup -> CompiledCode (BuiltinData -> BuiltinUnit)
rollupCompiled computation =
    $$(compile [|| untypedRollup ||])
    `unsafeApplyCode` liftCodeDef computation

parkingSpotCompiled :: Integer -> CompiledCode (BuiltinData -> BuiltinUnit)
parkingSpotCompiled tag =
    $$(compile [|| untypedParkingSpot ||])
    `unsafeApplyCode` liftCodeDef tag
