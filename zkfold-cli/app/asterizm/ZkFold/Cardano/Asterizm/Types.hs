module ZkFold.Cardano.Asterizm.Types where

import           Data.Aeson                    (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Base16        as B16
import qualified Data.Text.Encoding            as TE
import           GeniusYield.Types             (mintingPolicyIdToCurrencySymbol)
import qualified PlutusLedgerApi.V3            as V3
import           Prelude

import           ZkFold.Cardano.Asterizm.Utils (bsToHex, hexToBS, policyFromPlutus)
import           ZkFold.Cardano.UPLC.Asterizm  (AsterizmSetup (..), asterizmRelayThreadPolicy)


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
  { acsClientPKH    = clientPKH
  , acsThreadSymbol = V3.CurrencySymbol $ V3.toBuiltin cs
  , acsClientSymbol = mintingPolicyIdToCurrencySymbol clientPolicyId
  , acsClientFee    = 10_000_000  --ToDo: this value should be configurable
  }
  where
    clientPKH      = V3.PubKeyHash $ V3.toBuiltin pkh
    clientPolicyId = snd . policyFromPlutus $ asterizmRelayThreadPolicy clientPKH


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


