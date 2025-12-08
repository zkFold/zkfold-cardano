{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Cardano.UPLC.RollupSimple.Types (
  RollupState (..),
  RollupSimpleRed (..),
  BridgeUtxoStatus (..),
  BridgeUtxoInfo (..),
  RollupConfiguration (..),
) where

import           GHC.Generics                        (Generic)
import           PlutusLedgerApi.V3                  (Address, CurrencySymbol, TokenName, TxOutRef)
import           PlutusTx.Blueprint
import qualified PlutusTx.Blueprint.TH
import           PlutusTx.Prelude                    hiding (toList, (*), (+))
import           Prelude                             (Show)

import           ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes, SetupBytes)

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

data RollupSimpleRed = RollupSimpleRed
  { rsrProofBytes :: ProofBytes
  -- ^ Proof for state update.
  , rsrAddress    :: Address
  -- ^ Address of the spending validator.
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''RollupSimpleRed [('RollupSimpleRed, 0)]

data BridgeUtxoStatus
  = -- | New UTxO being bridged in, also giving it's layer-2 address.
    BridgeIn Integer
  | -- | UTxO being bridged out.
    BridgeOut
  | -- | Already bridged in UTxO is getting updated, usually for satisfying bridge-out requirement.
    BridgeBalance
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''BridgeUtxoStatus [('BridgeIn, 0), ('BridgeOut, 1), ('BridgeBalance, 2)]

data BridgeUtxoInfo = BridgeUtxoInfo
  { buiORef   :: TxOutRef
  -- ^ Reference to the state UTxO being updated.
  , buiStatus :: BridgeUtxoStatus
  -- ^ Status of the UTxO.
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''BridgeUtxoInfo [('BridgeUtxoInfo, 0)]

data RollupConfiguration = RollupConfiguration
  { rcNftCurrencySymbol :: CurrencySymbol
  -- ^ NFT Currency Symbol.
  , rcNftTokenName      :: TokenName
  -- ^ NFT Token Name.
  , rcSetupBytes        :: SetupBytes
  -- ^ Setup bytes.
  , rcMaxBridgeIn       :: Integer
  -- ^ Maximum number of UTxOs that can be bridged in.
  , rcMaxBridgeOut      :: Integer
  -- ^ Maximum number of UTxOs that can be bridged out.
  , rcMaxOutputAssets   :: Integer
  -- ^ Maximum number of assets that can be present in the layer-2 output.
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''RollupConfiguration [('RollupConfiguration, 0)]
