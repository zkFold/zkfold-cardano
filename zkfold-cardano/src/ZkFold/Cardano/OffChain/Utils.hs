module ZkFold.Cardano.OffChain.Utils where

import           Cardano.Api          hiding (Lovelace)
import           Cardano.Api.Ledger   (toCBOR)
import           Cardano.Api.Shelley  (PlutusScript (..), fromPlutusData, scriptDataFromJsonDetailedSchema,
                                       scriptDataToJsonDetailedSchema, shelleyPayAddrToPlutusPubKHash, toPlutusData)
import           Codec.CBOR.Write     (toStrictByteString)
import           Control.Monad        (void, zipWithM)
import           Data.Aeson           (decode, eitherDecode, parseJSON, (.:), (.:?))
import qualified Data.Aeson.Key       as Key
import qualified Data.Aeson.KeyMap    as KeyMap
import           Data.Aeson.Types     as Aeson (Result (..), Value (..), parse, parseEither)
import           Data.Bifunctor       (first)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Either
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           PlutusLedgerApi.V3   as V3
import           PlutusTx             (CompiledCode)
import           Prelude              hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           Text.Parsec          (many1)
import           Text.Parsec.Char     (digit)
import           Text.Parsec.String   (Parser)
import           Text.Printf          (printf)
import           Text.Read            (readEither)

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

-- Function to parse JSON and convert to a list of TxInInfo
parseJsonToTxInInfoList :: [Maybe (CompiledCode a)] -> BL.ByteString -> Either String [TxInInfo]
parseJsonToTxInInfoList mscripts jsonInput = do
    jsonData <- eitherDecode jsonInput
    case jsonData of
      Object utxoMap -> zipWithM parseEntry mscripts (KeyMap.toList utxoMap)
      _              -> Left "Expected top-level JSON object"

  where
    parseEntry :: Maybe (CompiledCode a) -> (KeyMap.Key, Aeson.Value) -> Either String TxInInfo
    parseEntry mscript (txRef, Object utxoInfo) = do
        -- Parse TxOutRef from txRef (e.g., "3dfe...#0")
        (txIdHex, txIxStr) <- case T.splitOn "#" (Key.toText txRef) of
              [txIdHex, txIxStr] -> Right (txIdHex, txIxStr)
              _                  -> Left "Failed to parse TxOutRef"
        txId <- case deserialiseFromRawBytesHex AsTxId (TE.encodeUtf8 txIdHex) of
          Right txId' -> Right . V3.TxId . toBuiltin . serialiseToRawBytes $ txId'
          Left err    -> Left $ "Failed to parse TxId: " ++ show err
        txIx <- first (const "Failed to parse TxIx") (readEither (T.unpack txIxStr))
        let txOutRef = TxOutRef txId txIx

        -- Parse Address
        addressText <- parseEither (.: "address") utxoInfo
        address     <- case mscript of
          Nothing     -> parseAddress addressText
          Just script -> Right $ V3.Address (credentialOf script) Nothing

        -- Parse Value (assumes only "lovelace" entry.  TODO: generalise)
        valueObj       <- parseEither (.: "value") utxoInfo
        lovelaceAmount <- parseEither (.: "lovelace") valueObj
        let value = V3.singleton adaSymbol adaToken lovelaceAmount

        -- -- Parse Datum
        -- datum <- case utxoInfo .:? "datum" of
        --     Just (Aeson.String datumStr) -> Right (OutputDatum (Datum $ toBuiltinData datumStr))
        --     _                            -> Right OutputDatumNone

        -- Parse InlineDatum
        inlineDatum <- case parseEither (.:? "inlineDatum") utxoInfo of
                         Right (Just inlineDatumObject) -> parseInlineDatum inlineDatumObject
                         Right Nothing                  -> Right Nothing
                         Left err                       -> Left $ "Failed to parse inlineDatum: " ++ err

        -- Parse ReferenceScript
        referenceScript <- case parseEither (.:? "referenceScript") utxoInfo of
            Right (Just refScriptObject) -> parseReferenceScript refScriptObject
            Right Nothing                -> Right Nothing
            Left err                     -> Left $ "Failed to parse referenceScript: " ++ err

        -- Create TxOut and TxInInfo
        let txOut = V3.TxOut
              { txOutAddress         = address
              , txOutValue           = value
              , txOutDatum           = fromMaybe NoOutputDatum inlineDatum
              , txOutReferenceScript = referenceScript
              }
        return $ TxInInfo txOutRef txOut

    parseEntry _ _ = Left "Failed to parse TxInInfo entry"

-- Helper to parse inline datum
parseInlineDatum :: Aeson.Value -> Either String (Maybe OutputDatum)
parseInlineDatum v =
    let datE = scriptDataFromJsonDetailedSchema v

    in case datE of
      Right dat -> (Right . Just . OutputDatum . Datum . dataToBuiltinData . toPlutusData . getScriptData $ dat)
      Left _    -> Left "JSON error: scriptdata json error"

-- Helper to parse reference script
parseReferenceScript :: Aeson.Value -> Either String (Maybe (V3.ScriptHash))
parseReferenceScript (Object obj) = do
    scriptObj <- case parseEither (.:? "script") obj of
        Right (Just script) -> Right (Just script)
        Right Nothing       -> Right Nothing
        Left err            -> Left $ "Failed to parse 'script' object: " ++ err
    case scriptObj of
        Just (Object script) -> do
            cborHex <- case parseEither (.:? "cborHex") script of
                Right (Just (String cborHexStr)) -> Right (TE.encodeUtf8 cborHexStr)
                Right (Just _)                   -> Left "Failed to parse 'cborHex'"
                Right Nothing                    -> Left "Missing 'cborHex' in reference script"
                Left err                         -> Left $ "Failed to parse 'cborHex': " ++ err
            -- Decode CBOR hex bytes
            cborHSD <- either (const (Left "Invalid CBOR hex")) Right $
              deserialiseFromCBOR AsHashableScriptData cborHex
            return $ Just . V3.ScriptHash . V3.toBuiltin . serialiseToRawBytes . hashScriptDataBytes $ cborHSD
        _                    -> Right Nothing
parseReferenceScript _ = Right Nothing

-- Experimental function to parse and display a TxIn
displayTxIn :: BL.ByteString -> Either String Cardano.Api.TxIn
displayTxIn jsonInput = do
    txIn <- case decode jsonInput of
        Just value -> case parse parseJSON value :: Result TxIn of
                        Success txIn -> Right txIn
                        Error err    -> Left $ "Failed to parse TxIn: " ++ err
        Nothing    -> Left "Invalid JSON input"
    return txIn

-- | Compare function for 'TxOutRef'
outRefCompare :: TxOutRef -> TxOutRef -> Ordering
outRefCompare o1 o2 =
    case compare (txOutRefId o1) (txOutRefId o2) of
        EQ  -> compare (txOutRefIdx o1) (txOutRefIdx o2)
        ord -> ord
