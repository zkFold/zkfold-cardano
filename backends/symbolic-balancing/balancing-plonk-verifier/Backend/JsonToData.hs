{-# LANGUAGE OverloadedStrings #-}

module Backend.JsonToData where

import           Cardano.Api
import           Cardano.Api.Shelley  (PlutusScript (..), scriptDataFromJsonDetailedSchema,
                                       shelleyPayAddrToPlutusPubKHash, toPlutusData)
import           Control.Monad        (zipWithM)
import           Data.Aeson           (eitherDecode, parseJSON, (.:), (.:?))
import qualified Data.Aeson.Key       as Key
import qualified Data.Aeson.KeyMap    as KeyMap
import           Data.Aeson.Types     as Aeson (Value (..), parseEither)
import           Data.Bifunctor       (first)
import qualified Data.ByteString.Lazy as BL
import           Data.Either
import           Data.Maybe           (Maybe (..), fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           PlutusLedgerApi.V3   as V3
import           PlutusTx             (CompiledCode)
import           Prelude              (String, const, maybe, return, show, ($), (++), (.))
import           Text.Read            (readEither)

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
            -- Right (Just refScriptObject) -> parseReferenceScript refScriptObject
            Right (Just refScriptObject) -> do
              sh <- parseEither (parseJSON @Cardano.Api.ScriptHash) refScriptObject
              return . Just . V3.ScriptHash . V3.toBuiltin . serialiseToRawBytes $ sh
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

-- Helper to parse address in era
parseAddress :: Text -> Either String V3.Address
parseAddress addressText = do
    shellyAddr <- either (const $ Left "Failed to parse Shelly address") Right $
                  deserialiseFromBech32 (AsAddress AsShelleyAddr) addressText
    pkh        <- maybe (Left "Failed to parse address pubkey hash") Right $
                  shelleyPayAddrToPlutusPubKHash shellyAddr
    return $ Address (PubKeyCredential pkh) Nothing

-- Helper to parse inline datum
parseInlineDatum :: Aeson.Value -> Either String (Maybe OutputDatum)
parseInlineDatum v =
    let datE = scriptDataFromJsonDetailedSchema v

    in case datE of
      Right dat -> (Right . Just . OutputDatum $ (unsafeFromData . toPlutusData . getScriptData $ dat :: Datum))
      Left _    -> Left "JSON error: scriptdata json error"

-- | Credential of compiled script
credentialOf :: CompiledCode a -> V3.Credential
credentialOf = V3.ScriptCredential . V3.ScriptHash . V3.toBuiltin . serialiseToRawBytes . hashScript
               . PlutusScript plutusScriptVersion . PlutusScriptSerialised @PlutusScriptV3 . V3.serialiseCompiledCode
