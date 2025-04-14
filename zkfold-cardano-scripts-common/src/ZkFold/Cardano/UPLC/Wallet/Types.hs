{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Cardano.UPLC.Wallet.Types (
  Web2Creds (..),
  JWTParts (..),
  Web2Auth (..),
  Signature (..),
) where

import           GHC.Generics                        (Generic)
import           PlutusLedgerApi.V3
import           PlutusTx.Blueprint
import qualified PlutusTx.Blueprint.TH
import           PlutusTx.Prelude                    hiding (toList, (*), (+))
import           Prelude                             (Show)

import           ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes)

newtype Web2Creds = Web2Creds
  { w2cEmail :: BuiltinByteString
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''Web2Creds [('Web2Creds, 0)]

data JWTParts = JWTParts
  { jwtPrefix :: BuiltinByteString
  , jwtSuffix :: BuiltinByteString
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''JWTParts [('JWTParts, 0)]

data Web2Auth = Web2Auth JWTParts ProofBytes TokenName
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''Web2Auth [('Web2Auth, 0)]

data Signature = Signature Integer Integer
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''Signature [('Signature, 0)]
