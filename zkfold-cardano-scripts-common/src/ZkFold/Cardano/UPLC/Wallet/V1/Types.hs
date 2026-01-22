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

import           GHC.Generics                        (Generic)
import           PlutusLedgerApi.V3
import           PlutusTx.Blueprint
import qualified PlutusTx.Blueprint.TH
import           PlutusTx.Prelude                    hiding (toList, (*), (+))
import           Prelude                             (Show)

import           ZkFold.Cardano.UPLC.Wallet.V0.Types (JWTParts (..), KeyId (..), OnChainWalletConfig (..))

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
