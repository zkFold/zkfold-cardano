{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Cardano.UPLC.Wallet.Types (
  JWTParts (..),
  KeyId (..),
  Web2Auth (..),
  Signature (..),
  OnChainWalletConfig (..),
) where

import           GHC.Generics                        (Generic)
import           PlutusLedgerApi.V3
import           PlutusTx.Blueprint
import qualified PlutusTx.Blueprint.TH
import           PlutusTx.Prelude                    hiding (toList, (*), (+))
import           Prelude                             (Show)

import           ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes)

data JWTParts = JWTParts
  { jwtHeader :: BuiltinByteString
  , jwtPrefix :: BuiltinByteString
  , jwtSuffix :: BuiltinByteString
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''JWTParts [('JWTParts, 0)]

newtype KeyId = KeyId
  { keyId :: BuiltinByteString
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''KeyId [('KeyId, 0)]

data Web2Auth = Web2Auth JWTParts ProofBytes TokenName KeyId
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''Web2Auth [('Web2Auth, 0)]

data Signature = Signature Integer Integer
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''Signature [('Signature, 0)]

data OnChainWalletConfig = OnChainWalletConfig
  { ocwcBeaconPolicyId :: CurrencySymbol
  -- ^ Beacon token minting policy
  , ocwcBeaconName     :: TokenName
  -- ^ Beacon token name
  , ocwcUidPrefix      :: BuiltinByteString
  -- ^ User ID prefix. It is the name of the field in the JWT that identifies the user:
  -- "email" for Google or "sub" for Epic Games
  , ocwcUid            :: BuiltinByteString
  -- ^ User ID from the JWT
  , ocwcFeeAddress     :: Address
  -- ^ zkFold address where an additional fee will be sent
  , ocwcFee            :: Integer
  -- ^ The additional fee amount
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''OnChainWalletConfig [('OnChainWalletConfig, 0)]
