module ZkFold.Cardano.OffChain.Utils where

import           Cardano.Api         hiding (Lovelace)
import           Cardano.Api.Ledger  (toCBOR)
import           Cardano.Api.Shelley (PlutusScript (..), fromPlutusData, scriptDataToJsonDetailedSchema, shelleyPayAddrToPlutusPubKHash)
import           Codec.CBOR.Write    (toStrictByteString)
import           Control.Monad       (void)
import qualified Data.Aeson          as Aeson
import qualified Data.ByteString     as BS
import           PlutusTx            (CompiledCode)
import           PlutusTx.Prelude    ((.))
import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as TE
import           Text.Parsec                             (many1)
import           Text.Parsec.Char                        (digit)
import           Text.Parsec.String                      (Parser)
import           Data.Bifunctor                          (first)
import           Text.Read                               (readEither)
import           Prelude             hiding (Bool, Eq (..), Fractional (..), Num (..), length, (.))
import           Text.Printf                                 (printf)
import           PlutusLedgerApi.V3                          as V3

-- | Write serialized script to a file.
writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

-- | Serialize plutus script.
savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . serialiseCompiledCode

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . toData

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . toData

-- | Credential of compiled validator script
credentialOf :: CompiledCode a -> V3.Credential
credentialOf = ScriptCredential . V3.ScriptHash . toBuiltin . serialiseToRawBytes . hashScript
               . PlutusScript plutusScriptVersion . PlutusScriptSerialised @PlutusScriptV3 . serialiseCompiledCode

-- | Currency symbol of compiled minting script
currencySymbolOf :: CompiledCode a -> V3.CurrencySymbol
currencySymbolOf = CurrencySymbol . toBuiltin . serialiseToRawBytes . hashScript
                   . PlutusScript plutusScriptVersion . PlutusScriptSerialised @PlutusScriptV3 . serialiseCompiledCode

-- | Parser for a positive integer
integerParser :: Parser Integer
integerParser = do
  digits <- many1 digit
  return $ read digits

-- | Parse TxOutRef
parseTxOutRef :: String -> Either String V3.TxOutRef
parseTxOutRef orefStr = do
  (txIdHex, txIxStr) <- case T.splitOn (T.pack "#") (T.pack orefStr) of
    [txIdHex, txIxStr] -> Right (txIdHex, txIxStr)
    _                  -> Left "Failed to parse TxOutRef"
  txId <- case deserialiseFromRawBytesHex AsTxId (TE.encodeUtf8 txIdHex) of
    Right txId' -> Right . V3.TxId . toBuiltin . serialiseToRawBytes $ txId'
    Left err    -> Left $ "Failed to parse TxId: " ++ show err
  txIx <- first (const "Failed to parse TxIx") (readEither (T.unpack txIxStr))
  return $ TxOutRef txId txIx

-- | Parse address in era
parseAddress :: String -> Either String V3.Address
parseAddress addressStr = do
    shellyAddr <- either (const $ Left "Failed to parse Shelly address") Right $
                  deserialiseFromBech32 (AsAddress AsShelleyAddr) (T.pack addressStr)
    pkh        <- maybe (Left "Failed to parse address pubkey hash") Right $
                  shelleyPayAddrToPlutusPubKHash shellyAddr
    return $ V3.Address (PubKeyCredential pkh) Nothing

-- | Get hex representation of bytestring
byteStringAsHex :: BS.ByteString -> String
byteStringAsHex bs = concat $ BS.foldr' (\w s -> (printf "%02x" w):s) [] bs

-- | Script hash of compiled validator
scriptHashOf :: CompiledCode a -> V3.ScriptHash
scriptHashOf = V3.ScriptHash . toBuiltin . serialiseToRawBytes . hashScript . PlutusScript plutusScriptVersion
               . PlutusScriptSerialised @PlutusScriptV3 . serialiseCompiledCode