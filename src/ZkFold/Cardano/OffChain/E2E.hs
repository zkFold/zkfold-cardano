{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.OffChain.E2E where

import           Data.Aeson                                  (FromJSON, ToJSON)
import           GHC.Generics                                (Generic)
import           PlutusLedgerApi.V3                          (BuiltinByteString)
import           PlutusTx                                    (makeIsDataIndexed)
import           Prelude                                     (Show)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import qualified ZkFold.Base.Data.Vector                     as V
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret
import           ZkFold.Cardano.UPLC.Rollup                  (RollupRedeemer)

-- These types can only be used for testing.

data EqualityCheckContract = EqualityCheckContract {
    x           :: Fr
  , ps          :: PlonkupProverSecret BLS12_381_G1
  , targetValue :: Fr
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data IdentityCircuitContract = IdentityCircuitContract {
    x'  :: Fr
  , ps' :: PlonkupProverSecret BLS12_381_G1
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving newtype instance FromJSON (V.Vector 19 Fr)
deriving anyclass instance ToJSON   (PlonkupProverSecret BLS12_381_G1)
deriving anyclass instance FromJSON (PlonkupProverSecret BLS12_381_G1)

data RollupInfo = RollupInfo { riDataUpdate :: [[BuiltinByteString]]
                             , riProtoState :: BuiltinByteString
                             , riRedeemer   :: RollupRedeemer
                             } deriving stock (Show, Generic)

makeIsDataIndexed ''RollupInfo [('RollupInfo,0)]

