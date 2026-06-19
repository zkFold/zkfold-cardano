{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Cardano.UPLC.RollupSimple.Types (
  RollupState (..),
  RollupSimpleRed (..),
  BridgeUtxoStatus (..),
  BridgeUtxoInfo (..),
  RollupConfiguration (..),
) where

import           GHC.Generics              (Generic)
import           Plutus.Crypto.Halo2.Proof (Proof)
import           PlutusLedgerApi.V3        (Address, CurrencySymbol, TokenName, TxOutRef)
import           PlutusTx.Blueprint
import qualified PlutusTx.Blueprint.TH
import           PlutusTx.Prelude          hiding (toList, (*), (+))
import           Prelude                   (Show)

data RollupState = RollupState
  { previousStateHash :: Integer
  , utxoTreeRoot      :: Integer
  , chainLength       :: Integer
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''RollupState [('RollupState, 0)]

data RollupSimpleRed = RollupSimpleRed
  { rsrProofBytes :: Proof
  -- ^ Proof for state update.
  , rsrAddress    :: Address
  -- ^ Address of the spending validator.
  , rsrDelta      :: [Integer]
  -- ^ Tree delta: flattened list of field elements encoding Merkle tree leaf changes.
  -- Structure: [bi*(isActive, position, newHash)] ++ [t*n*position] ++ [t*n*(isActive, position, newHash)]
  -- The ZK proof binds this data to the state transition, so a wrong delta fails verification.
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
  | -- | Initial bridge-in UTxO created by user, waiting to be processed by aggregator.
    BridgeInInitial Integer
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''BridgeUtxoStatus [('BridgeIn, 0), ('BridgeOut, 1), ('BridgeBalance, 2), ('BridgeInInitial, 3)]

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
