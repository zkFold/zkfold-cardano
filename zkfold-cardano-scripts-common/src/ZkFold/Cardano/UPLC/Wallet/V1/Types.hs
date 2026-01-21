{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Cardano.UPLC.Wallet.V1.Types (
  JWTParts (..),
  KeyId (..),
  PubKey (..),
  UserId (..),
  SigmaProof (..),
  RewardingRedeemer (..),
  OnChainWalletConfig (..),
) where

import ZkFold.Cardano.UPLC.Wallet.V0.Types (JWTParts(..), KeyId(..))
import           GHC.Generics                        (Generic)
import Prelude (Show)
import           PlutusLedgerApi.V3
import           PlutusTx.Blueprint
import qualified PlutusTx.Blueprint.TH
import           PlutusTx.Prelude                    hiding (toList, (*), (+))

data PubKey = PubKey { pubE :: Integer, pubN :: Integer }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''PubKey [('PubKey, 0)]

newtype UserId = UserId { userId :: BuiltinByteString }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''UserId [('UserId, 0)]

data SigmaProof = SigmaProof { v :: [Integer], aut :: [Integer] }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''SigmaProof [('SigmaProof, 0)]

data RewardingRedeemer = RewardingRedeemer JWTParts UserId SigmaProof KeyId
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''RewardingRedeemer [('RewardingRedeemer, 0)]

data OnChainWalletConfig = OnChainWalletConfig
  { ocwcUidPrefix      :: BuiltinByteString
  -- ^ User ID prefix. It is the name of the field in the JWT that identifies the user:
  -- "email" for Google or "sub" for Epic Games
  , ocwcFeeAddress     :: Address
  -- ^ zkFold address where an additional fee will be sent
  , ocwcFee            :: Integer
  -- ^ The additional fee amount
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''OnChainWalletConfig [('OnChainWalletConfig, 0)]

