{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ZkFold.Cardano.Asterizm.Types where

import           Data.Aeson                    (FromJSON (..), ToJSON (..), withText)
import qualified Data.ByteString               as BS
import           GeniusYield.Types             (GYMintingPolicyId, GYPubKeyHash)
import           GHC.Generics                  (Generic)
import           Prelude

import           ZkFold.Cardano.Asterizm.Utils (bsToHex, hexToBS)

-- | Setup parameters for @asterizmClient@
data AsterizmSetup = AsterizmSetup
  { acsClientPKH       :: GYPubKeyHash
  , acsAllowedRelayers :: [GYMintingPolicyId]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Asterizm cross-chain message (hex-encoded for JSON serialization).
newtype AsterizmMessage = AsterizmMessage { unAsterizmMessage :: BS.ByteString }

instance ToJSON AsterizmMessage where
  toJSON (AsterizmMessage bs) = toJSON $ bsToHex bs

instance FromJSON AsterizmMessage where
  parseJSON = withText "AsterizmMessage" (fmap AsterizmMessage . hexToBS)


