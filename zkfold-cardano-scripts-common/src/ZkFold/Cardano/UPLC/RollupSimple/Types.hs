{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Cardano.UPLC.RollupSimple.Types (
  RollupState (..),
  RollupSimpleRed (..),
) where

import           GHC.Generics                        (Generic)
import           PlutusTx.Blueprint
import qualified PlutusTx.Blueprint.TH
import           PlutusTx.Prelude                    hiding (toList, (*), (+))
import           Prelude                             (Show)

import           ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes)

data RollupState = RollupState
  { previousStateHash   :: Integer
  , utxoTreeRoot        :: Integer
  , chainLength         :: Integer
  , bridgeInCommitment  :: Integer
  , bridgeOutCommitment :: Integer
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''RollupState [('RollupState, 0)]

newtype RollupSimpleRed = RollupSimpleRed ProofBytes
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''RollupSimpleRed [('RollupSimpleRed, 0)]
