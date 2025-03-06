module ZkFold.Cardano.Rollup.Transaction.Clear where
import           Cardano.Api                    hiding (Key, Lovelace, TxOut)
import           Control.Applicative            (Applicative (..), (<$>))
import           Control.Monad                  (Functor (..), Monad (..))
import           Data.Aeson                     (FromJSON (..), Key, Object, Value (..), eitherDecode, withObject)
import           Data.Aeson.Key                 (toText)
import           Data.Aeson.KeyMap              (toList)
import           Data.Aeson.Types               (Parser, parseMaybe, (.!=), (.:), (.:?))
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Data.Either                    (Either (..))
import           Data.List                      (concat, concatMap, length, replicate, unlines, zipWith)
import           Data.Maybe                     (Maybe (..), fromMaybe)
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import           Data.Tuple                     (fst)
import           Prelude                        (IO, Int, Num (..), Show (..), String, error, ($), (++), (.))
import           System.FilePath                (FilePath, (</>))
import qualified System.IO                      as IO

import           ZkFold.Cardano.OffChain.Utils  (dataToCBOR, scriptHashOf)
import           ZkFold.Cardano.UPLC.RollupData (RollupDataRedeemer (..), rollupDataCompiled)

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

newtype TokenValue = TokenValue
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


-- Due to lack of composability of transactions built with cardano-cli, we need to dynamically
-- generate cardano-cli code for transactions with variable inputs and/or variable minting
-- parameters.

initialCodeBlock :: [String]
initialCodeBlock =
  [ "#! /bin/bash"
  , "set -e"
  , "set -u"
  , "set -o pipefail"
  , "assets=../assets"
  , "keypath=./rollup/keys"
  , "privpath=./rollup/priv"
  , "mN=$(cat $privpath/testnet.flag)"
  , "unitDatum=$assets/unit.cbor"
  , "unitRedeemer=$assets/unit.cbor"
  , "dataPolicy=$assets/rollupData.plutus"
  , "dataCleanRedeemer=$assets/dataCleanRedeemer.cbor"
  , "parkingSpotPolicy=$assets/parkingSpot.plutus"
  , "collateral=$(cardano-cli conway transaction txid --tx-file \"$keypath/splitAlice.tx\")#0"
  , ""
  , "in1=$(cardano-cli conway transaction txid --tx-file \"$keypath/rollupOut.tx\")#3"
  , "dataPolicyId=$(cardano-cli conway transaction policyid --script-file $dataPolicy)"
  , ""
  , "cardano-cli conway transaction build \\"
  , "  --testnet-magic $mN \\"
  , "  --tx-in $in1 \\"
  ]

middleCodeBlock :: [String]
middleCodeBlock =
  [ "  --tx-in-collateral $collateral \\"
  , "  --change-address $(cat $keypath/alice.addr) \\"
  ]

finalCodeBlock :: [String]
finalCodeBlock =
  [ "  --mint-script-file $dataPolicy \\"
  , "  --mint-redeemer-cbor-file $dataCleanRedeemer \\"
  , "  --out-file $keypath/dataClean.txbody"
  ]

inputCodeBlock :: String -> [String]
inputCodeBlock s =
  [ "  --tx-in " ++ s ++ " \\"
  , "  --tx-in-script-file $parkingSpotPolicy \\"
  , "  --tx-in-inline-datum-present \\"
  , "  --tx-in-redeemer-cbor-file $unitRedeemer \\"
  ]

mintCodeBlock :: [String] -> [String]
mintCodeBlock tokenNames = ["  --mint \"" ++ concat (zipWith zipper tokenNames wrapUps)]
  where
    zipper tn s = "-1 $dataPolicyId"  ++ "." ++ tn ++ s
    wrapUps     = replicate (length tokenNames - 1) " + " ++ ["\" \\"]


-- | Extract keys and token names from parsed UTxOs
extractKeysAndTokens :: [Utxo] -> ([String], [String])
extractKeysAndTokens utxos  =
    let keys       = fmap utxoKey utxos
        tokenNames = concatMap (fmap fst . tokens . txOutValue . utxoValue) utxos
    in (keys, tokenNames)

rollupClear :: FilePath -> IO ()
rollupClear path = do
    let assets = path </> "assets"

    BS.writeFile (assets </> "dataCleanRedeemer.cbor") . dataToCBOR $ OldData

    content <- BL.readFile $ assets </> "dataTokensBurn.json"

    utxos <- case eitherDecode content of
      Left err    -> error $ "Error parsing JSON: " ++ err
      Right utxos -> return utxos

    let (keys, tokenNames) = extractKeysAndTokens utxos
        inputs = concatMap inputCodeBlock keys
        mints  = mintCodeBlock tokenNames

    IO.putStrLn $ "\nBurning used data tokens: " ++ show tokenNames ++ "\n"

    IO.writeFile (assets </> "burnDataTokens.sh") . unlines
      $ initialCodeBlock ++ inputs ++ middleCodeBlock ++ mints ++ finalCodeBlock
