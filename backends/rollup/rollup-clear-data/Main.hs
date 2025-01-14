{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cardano.Api                    hiding (Key, Lovelace, TxOut)
import           Cardano.Api.Ledger             (toCBOR)
import           Cardano.Api.Shelley            (PlutusScript (..), fromPlutusData) 
import           Codec.CBOR.Write               (toStrictByteString)
import           Data.Aeson
import           Data.Aeson.Key                 (toText)
import           Data.Aeson.KeyMap              (toList)
import           Data.Aeson.Types               (Parser, parseMaybe) 
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Data.Maybe                     (Maybe (..), fromMaybe)
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import qualified PlutusLedgerApi.V3             as V3
import           PlutusTx                       (CompiledCode, ToData, toData)
import           Prelude                        (Either (..), Int, IO, Show, String, concatMap, fst, map, putStrLn,
                                                 return, show, unlines, ($), (<$>), (<*>), (++), (==), (.))
import           System.Directory               (getCurrentDirectory)
import           System.FilePath                (takeFileName, (</>))
import qualified System.IO                      as IO

import qualified Rollup.CardanoCli              as Cli
import           ZkFold.Cardano.UPLC            (rollupDataCompiled)
import           ZkFold.Cardano.UPLC.RollupData (RollupDataRedeemer (..))

data Utxo = Utxo
  { utxoKey :: String
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
          Nothing -> []
          Just obj -> [(T.unpack (toText k), 1) | (k, _) <- toList obj]
    return $ TokenValue tokens

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let currentDirName = takeFileName currentDir
      path           = if currentDirName == "rollup" then (".." </> "..")
                          else if currentDirName == "e2e-test" then ".." else "."
      assetsPath     = path </> "assets"

  BS.writeFile (assetsPath </> "dataCleanRedeemer.cbor") . dataToCBOR $ OldData

  content <- BL.readFile $ assetsPath </> "dataTokensBurn.json"

  case eitherDecode content of
    Left err    -> putStrLn $ "Error parsing JSON: " ++ err
    Right utxos -> do
      let (keys, tokenNames) = extractKeysAndTokens utxos
          inputs = concatMap Cli.inputCodeBlock keys
          mints  = Cli.mintCodeBlock tokenNames

      putStrLn $ "\nBurning used data tokens: " ++ show tokenNames ++ "\n"

      IO.writeFile (assetsPath </> "burnDataTokens.sh") . unlines
        $ Cli.initialCodeBlock ++ inputs ++ Cli.middleCodeBlock ++ mints ++ Cli.finalCodeBlock


----- HELPER FUNCTIONS -----

-- | Script hash of compiled validator
scriptHashOf :: CompiledCode a -> V3.ScriptHash
scriptHashOf = V3.ScriptHash . V3.toBuiltin . serialiseToRawBytes . hashScript . PlutusScript plutusScriptVersion
               . PlutusScriptSerialised @PlutusScriptV3 . V3.serialiseCompiledCode

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . toData

-- | Parse token values
parseTokenValue :: Object -> TokenValue
parseTokenValue obj = fromMaybe (TokenValue []) (parseMaybe parseJSON (Object obj))

-- | Extract keys and token names
extractKeysAndTokens :: [Utxo] -> ([String], [String])
extractKeysAndTokens utxoList = 
  let keys = map utxoKey utxoList
      tokenNames = concatMap (map fst . tokens . txOutValue . utxoValue) utxoList
  in (keys, tokenNames)
