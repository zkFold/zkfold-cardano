{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ZkFold.Cardano.Asterizm.Types where

import           Control.Monad                 ((<=<))
import           Data.Aeson                    (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import           Data.Bits                     (shiftL, shiftR)
import qualified Data.ByteString               as BS
import           Data.Word                     (Word64, Word8)
import           GHC.Generics                  (Generic)
import           Prelude

import           ZkFold.Cardano.Asterizm.Utils (bsToHex, hexToBS)

-- | Asterizm cross-chain message with structured header fields.
-- Header is 112 bytes: srcChainId (8) + srcAddress (32) + dstChainId (8) + dstAddress (32) + txId (32)
data AsterizmMessage = AsterizmMessage
  { amSrcChainId :: !Word64        -- ^ Source chain ID (8 bytes, big-endian)
  , amSrcAddress :: !BS.ByteString -- ^ Source address (32 bytes)
  , amDstChainId :: !Word64        -- ^ Destination chain ID (8 bytes, big-endian)
  , amDstAddress :: !BS.ByteString -- ^ Destination address (32 bytes)
  , amTxId       :: !BS.ByteString -- ^ Transaction ID (32 bytes)
  , amPayload    :: !BS.ByteString -- ^ Message payload (variable length)
  } deriving stock (Show, Eq)

instance ToJSON AsterizmMessage where
  toJSON msg = object
    [ "srcChainId" .= amSrcChainId msg
    , "srcAddress" .= bsToHex (amSrcAddress msg)
    , "dstChainId" .= amDstChainId msg
    , "dstAddress" .= bsToHex (amDstAddress msg)
    , "txId"       .= bsToHex (amTxId msg)
    , "payload"    .= bsToHex (amPayload msg)
    ]

instance FromJSON AsterizmMessage where
  parseJSON = withObject "AsterizmMessage" $ \v -> do
    srcChainId <- v .: "srcChainId"
    srcAddr    <- v .: "srcAddress" >>= hexToBS
    dstChainId <- v .: "dstChainId"
    dstAddr    <- v .: "dstAddress" >>= hexToBS
    txId       <- v .: "txId"       >>= hexToBS
    payload    <- v .: "payload"    >>= hexToBS
    pure $ AsterizmMessage srcChainId srcAddr dstChainId dstAddr txId payload

-- | Header length in bytes (srcChainId + srcAddress + dstChainId + dstAddress + txId)
headerLen :: Int
headerLen = 112

-- | Convert structured message to raw bytes (big-endian concatenation).
toByteString :: AsterizmMessage -> BS.ByteString
toByteString msg = mconcat
  [ word64ToBE (amSrcChainId msg)
  , amSrcAddress msg
  , word64ToBE (amDstChainId msg)
  , amDstAddress msg
  , amTxId msg
  , amPayload msg
  ]

-- | Parse raw bytes into structured message. Returns Nothing if < 112 bytes.
fromByteString :: BS.ByteString -> Maybe AsterizmMessage
fromByteString bs
  | BS.length bs < headerLen = Nothing
  | otherwise = Just $ AsterizmMessage
      { amSrcChainId = beToWord64 (BS.take 8 bs)
      , amSrcAddress = BS.take 32 (BS.drop 8 bs)
      , amDstChainId = beToWord64 (BS.take 8 (BS.drop 40 bs))
      , amDstAddress = BS.take 32 (BS.drop 48 bs)
      , amTxId       = BS.take 32 (BS.drop 80 bs)
      , amPayload    = BS.drop 112 bs
      }

-- | Convert Word64 to 8-byte big-endian ByteString
word64ToBE :: Word64 -> BS.ByteString
word64ToBE w = BS.pack [ fromIntegral (w `shiftR` (8 * i)) :: Word8 | i <- [7, 6 .. 0] ]

-- | Convert 8-byte big-endian ByteString to Word64
beToWord64 :: BS.ByteString -> Word64
beToWord64 bs = foldl (\acc b -> (acc `shiftL` 8) + fromIntegral b) 0 (BS.unpack $ BS.take 8 bs)

-- | Asterizm message hash (32 bytes, hex-encoded for JSON serialization).
newtype AsterizmMessageHash = AsterizmMessageHash { unAsterizmMessageHash :: BS.ByteString }
  deriving stock (Show, Eq)

instance ToJSON AsterizmMessageHash where
  toJSON (AsterizmMessageHash bs) = toJSON $ bsToHex bs

instance FromJSON AsterizmMessageHash where
  parseJSON = fmap AsterizmMessageHash . (hexToBS <=< parseJSON)

-- | Direction of cross-chain message transfer.
data MessageDirection
  = Incoming  -- ^ Message received from another chain (requires relayer verification)
  | Outgoing  -- ^ Message sent to another chain (no relayer verification needed)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert direction to Bool for on-chain script parameter.
directionToBool :: MessageDirection -> Bool
directionToBool Incoming = True
directionToBool Outgoing = False


