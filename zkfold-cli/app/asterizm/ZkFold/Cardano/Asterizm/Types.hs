module ZkFold.Cardano.Asterizm.Types where

import           Data.Aeson                    (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Base16        as B16
import qualified Data.Text.Encoding            as TE
import qualified PlutusLedgerApi.V3            as V3
import           Prelude

import           ZkFold.Cardano.Asterizm.Utils (bsToHex, hexToBS)
import           ZkFold.Cardano.UPLC.Asterizm  (AsterizmSetup (..))


data AsterizmParams = AsterizmParams
  { apClientPKH    :: ByteString
  , apThreadSymbol :: ByteString
  } deriving stock (Show)

instance ToJSON AsterizmParams where
  toJSON (AsterizmParams clientPKH threadSymbol) =
    object
      [ "apClientPKH"    .= bsToHex clientPKH
      , "apThreadSymbol" .= bsToHex threadSymbol
      ]

instance FromJSON AsterizmParams where
  parseJSON = withObject "AsterizmParams" $ \obj -> do
    clientPKHHex    <- obj .: "apClientPKH"
    threadSymbolHex <- obj .: "apThreadSymbol"

    clientPKH    <- hexToBS clientPKHHex
    threadSymbol <- hexToBS threadSymbolHex

    return $ AsterizmParams clientPKH threadSymbol

fromAsterizmParams :: AsterizmParams -> AsterizmSetup
fromAsterizmParams (AsterizmParams pkh cs) = AsterizmSetup
  { acsClientPKH    = V3.PubKeyHash $ V3.toBuiltin pkh
  , acsThreadSymbol = V3.CurrencySymbol $ V3.toBuiltin cs
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


