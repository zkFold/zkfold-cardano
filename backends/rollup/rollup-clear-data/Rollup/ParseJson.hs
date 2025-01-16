{-# LANGUAGE OverloadedStrings #-}

module Rollup.ParseJson where

import           Cardano.Api         hiding (Key, Lovelace, TxOut)
import           Cardano.Api.Shelley (PlutusScript (..))
import           Data.Aeson
import           Data.Aeson.Key      (toText)
import           Data.Aeson.KeyMap   (toList)
import           Data.Aeson.Types    (Parser, parseMaybe)
import           Data.Maybe          (Maybe (..), fromMaybe)
import           Data.String         (fromString)
import qualified Data.Text           as T
import qualified PlutusLedgerApi.V3  as V3
import           PlutusTx            (CompiledCode)
import           Prelude             (Int, Show, String, return, show, ($), (.), (<$>), (<*>))

import           ZkFold.Cardano.UPLC (rollupDataCompiled)


data Utxo = Utxo
  { utxoKey   :: String
  , utxoValue :: TxOut
  } deriving stock Show

data TxOut = TxOut
  { txOutAddress         :: String
  , txOutDatum           :: Maybe String
  , txOutInlineDatum     :: Maybe InlineDatum
  , txOutInlineDatumhash :: Maybe String
  , txOutReferenceScript :: Maybe String
  , txOutValue           :: TokenValue
  } deriving stock Show

data InlineDatum = InlineDatum
  { constructor :: Int
  , fields      :: [String]
  } deriving stock Show

data TokenValue = TokenValue
  { tokens :: [(String, Int)]
  } deriving stock Show

instance FromJSON Utxo where
  parseJSON = withObject "Utxo" $ \v ->
    Utxo <$> v .: "key" <*> v .: "value"

-- | Parse token values
parseTokenValue :: Object -> TokenValue
parseTokenValue obj = fromMaybe (TokenValue []) (parseMaybe parseJSON (Object obj))

instance FromJSON TxOut where
  parseJSON = withObject "TxOut" $ \v ->
    TxOut <$> v .: "address"
          <*> v .:? "datum"
          <*> v .:? "inlineDatum"
          <*> v .:? "inlineDatumhash"
          <*> v .:? "referenceScript"
          <*> (parseTokenValue <$> v .: "value")

instance FromJSON InlineDatum where
  parseJSON = withObject "InlineDatum" $ \v ->
    InlineDatum <$> v .: "constructor" <*> v .:? "fields" .!= []

thePolicyId :: Key
thePolicyId = fromString . show . scriptHashOf $ rollupDataCompiled

instance FromJSON TokenValue where
  parseJSON = withObject "TokenValue" $ \v -> do
    tokenMap <- v .:? thePolicyId :: Parser (Maybe Object)
    let tokens = case tokenMap of
          Nothing  -> []
          Just obj -> [(T.unpack (toText k), 1) | (k, _) <- toList obj]
    return $ TokenValue tokens


----- HELPER FUNCTIONS -----

-- | Script hash of compiled validator
scriptHashOf :: CompiledCode a -> V3.ScriptHash
scriptHashOf = V3.ScriptHash . V3.toBuiltin . serialiseToRawBytes . hashScript . PlutusScript plutusScriptVersion
               . PlutusScriptSerialised @PlutusScriptV3 . V3.serialiseCompiledCode
