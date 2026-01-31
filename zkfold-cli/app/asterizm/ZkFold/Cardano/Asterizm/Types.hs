module ZkFold.Cardano.Asterizm.Types where

import           Data.Aeson                    (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Base16        as B16
import qualified Data.Text.Encoding            as TE
import qualified PlutusLedgerApi.V3            as V3
import           Prelude

import           ZkFold.Cardano.Asterizm.Utils (bsToHex, hexToBS)
import           ZkFold.Cardano.UPLC.Asterizm  (AsterizmAdmin (..), AsterizmSetup (..))

data AsterizmClientParams = AsterizmClientParams
  { acpClientPKH    :: ByteString
  , acpThreadSymbol :: ByteString
  } deriving stock (Show)

instance ToJSON AsterizmClientParams where
  toJSON (AsterizmClientParams clientPKH threadSymbol) =
    object
      [ "apClientPKH"    .= bsToHex clientPKH
      , "apThreadSymbol" .= bsToHex threadSymbol
      ]

instance FromJSON AsterizmClientParams where
  parseJSON = withObject "AsterizmClientParams" $ \obj -> do
    clientPKHHex    <- obj .: "apClientPKH"
    threadSymbolHex <- obj .: "apThreadSymbol"

    clientPKH    <- hexToBS clientPKHHex
    threadSymbol <- hexToBS threadSymbolHex

    return $ AsterizmClientParams clientPKH threadSymbol

fromAsterizmClientParams :: AsterizmClientParams -> AsterizmSetup
fromAsterizmClientParams (AsterizmClientParams pkh cs) = AsterizmSetup
  { acsClientPKH    = V3.PubKeyHash $ V3.toBuiltin pkh
  , acsThreadSymbol = V3.CurrencySymbol $ V3.toBuiltin cs
  }

data AsterizmAdminParams = AsterizmAdminParams
  { aapAsterizmPKH :: ByteString
  , aapAsterizmFee :: Integer
  } deriving stock (Show)

instance ToJSON AsterizmAdminParams where
  toJSON (AsterizmAdminParams asterizmPKH asterizmFee) =
    object
      [ "aaAsterizmPKH" .= bsToHex asterizmPKH
      , "aaAsterizmFee" .= asterizmFee
      ]

instance FromJSON AsterizmAdminParams where
  parseJSON = withObject "AsterizmAdminParams" $ \obj -> do
    asterizmPKHHex <- obj .: "aaAsterizmPKH"
    asterizmFee    <- obj .: "aaAsterizmFee"

    asterizmPKH <- hexToBS asterizmPKHHex

    return $ AsterizmAdminParams asterizmPKH asterizmFee

fromAsterizmAdminParams :: AsterizmAdminParams -> AsterizmAdmin
fromAsterizmAdminParams (AsterizmAdminParams pkh fee) = AsterizmAdmin
  { aaAsterizmPKH = V3.PubKeyHash $ V3.toBuiltin pkh
  , aaAsterizmFee = V3.Lovelace fee
  }

-- | Hex-encoded ByteString wrapper.
newtype HexByteString = HexByteString ByteString
  deriving newtype (Show, Eq)

instance ToJSON HexByteString where
  toJSON (HexByteString bs) = toJSON (TE.decodeUtf8 $ B16.encode bs)

instance FromJSON HexByteString where
  parseJSON = withText "HexByteString" $ \t ->
    case B16.decode $ TE.encodeUtf8 t of
      Left err -> fail $ "Invalid hex string: " ++ err
      Right bs -> pure $ HexByteString bs


