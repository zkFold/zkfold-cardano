{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.RollupSimple (
  rollupSimple,
) where
import           Data.Function                       ((&))
import           GHC.Base                            (undefined)
import           GHC.Generics                        (Generic)
import           PlutusLedgerApi.V3
import           PlutusTx.Blueprint
import qualified PlutusTx.Builtins.Internal          as BI
import           PlutusTx.Prelude                    hiding (toList, (*), (+))
import           Prelude                             (Show)

import           ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes, SetupBytes)

data RollupState = RollupState
  { previousStateHash   :: Integer
  , utxoTreeRoot        :: Integer
  , length              :: Integer
  , bridgeInCommitment  :: Integer
  , bridgeOutCommitment :: Integer
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''RollupState [('RollupState, 0)]

data RollupSimpleRed = RollupSimpleRed RollupState ProofBytes
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''RollupSimpleRed [('RollupSimpleRed, 0)]

{-# INLINEABLE rollupSimple #-}
rollupSimple ::
  -- | Setup bytes.
  BuiltinData ->
  -- | Script context.
  BuiltinData ->
  BuiltinUnit
rollupSimple (unsafeFromBuiltinData -> (setupBytes :: SetupBytes)) scData = undefined
  where

    txInfoL = BI.unsafeDataAsConstr scData & BI.snd
    txInfo = txInfoL & BI.head & BI.unsafeDataAsConstr & BI.snd
    redL = txInfoL & BI.tail
    RollupSimpleRed rollupState proofBytes = redL & BI.head & unsafeFromBuiltinData
