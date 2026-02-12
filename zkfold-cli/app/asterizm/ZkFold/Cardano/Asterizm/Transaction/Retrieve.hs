{-# LANGUAGE OverloadedStrings #-}

module ZkFold.Cardano.Asterizm.Transaction.Retrieve where

import           Control.Exception             (throwIO)
import           Control.Monad                 (forM)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base16        as B16
import           Data.Char                     (isPrint)
import           Data.Coerce                   (coerce)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           GeniusYield.GYConfig          (Confidential (..), GYCoreConfig (..), GYCoreProviderInfo (..),
                                                withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Network.HTTP.Simple
import           PlutusLedgerApi.V3            (BuiltinByteString, fromBuiltin)
import           PlutusTx                      (unsafeFromBuiltinData)
import           Prelude
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (AsterizmMessage (..), AsterizmSetup (..), fromByteString)
import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import           ZkFold.Cardano.CLI.Parsers    (CoreConfigAlt, fromCoreConfigAltIO)
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmClientCompiled)

-- | Convert AsterizmSetup to policy id for querying (incoming messages).
setupToPolicyId :: AsterizmSetup -> GYMintingPolicyId
setupToPolicyId AsterizmSetup{..} = snd . policyFromPlutus $
  asterizmClientCompiled (pubKeyHashToPlutus acsClientPKH)
                         (mintingPolicyIdToCurrencySymbol <$> acsAllowedRelayers)
                         True  -- Incoming messages


-- Assumption: client's tokens are never consumed.

data Transaction = Transaction
  { curPath    :: !FilePath
  , coreCfgAlt :: !CoreConfigAlt
  }

fromNetworkIdIO :: GYNetworkId -> IO String
fromNetworkIdIO nid = case nid of
  GYMainnet        -> pure "mainnet"
  GYTestnetPreprod -> pure "preprod"
  GYTestnetPreview -> pure "preview"
  _                -> throwIO $ userError "Network not supported."

-- | Display a structured Asterizm message from datum
displayMsg :: GYOutDatum -> IO ()
displayMsg od = case od of
  GYOutDatumInline d -> do
    let plutusDatum = datumToPlutus' d
        rawBytes = fromBuiltin (unsafeFromBuiltinData plutusDatum :: BuiltinByteString)
    case fromByteString rawBytes of
      Just msg -> do
        putStrLn $ "  Source Chain ID: " ++ show (amSrcChainId msg)
        putStrLn $ "  Source Address:  " ++ bsToHexStr (amSrcAddress msg)
        putStrLn $ "  Dest Chain ID:   " ++ show (amDstChainId msg)
        putStrLn $ "  Dest Address:    " ++ bsToHexStr (amDstAddress msg)
        putStrLn $ "  Tx ID:           " ++ bsToHexStr (amTxId msg)
        putStrLn $ "  Payload (hex):   " ++ bsToHexStr (amPayload msg)
        putStrLn $ "  Payload (ASCII): " ++ bsToAscii (amPayload msg)
        putStrLn ""
      Nothing -> putStrLn "  (Invalid message: header too short)"
  _ -> putStrLn "  (Unexpected: no inline datum found)"

-- | Convert ByteString to hex string
bsToHexStr :: BS.ByteString -> String
bsToHexStr = T.unpack . TE.decodeUtf8 . B16.encode

-- | Convert ByteString to ASCII, replacing non-printable chars with '.'
bsToAscii :: BS.ByteString -> String
bsToAscii = map (\c -> if isPrint c then c else '.') . map (toEnum . fromIntegral) . BS.unpack

retrieveMsgs :: Transaction -> IO ()
retrieveMsgs (Transaction path coreCfg') = do
  coreCfg <- fromCoreConfigAltIO coreCfg'

  case cfgCoreProvider coreCfg of
    GYMaestro {} -> do
      let assetsPath = path </> "assets"
          setupFile  = assetsPath </> "asterizm-setup.json"

      mAsterizmSetup <- decodeFileStrict setupFile

      asterizmSetup <- case mAsterizmSetup of
        Just as -> pure as
        Nothing -> throwIO $ userError "Unable to decode Asterizm setup file."

      let policyId  = setupToPolicyId asterizmSetup
      let policyId' = trimQuot $ show policyId

      let nid = cfgNetworkId coreCfg
      nid' <- fromNetworkIdIO nid

      let requestUrl = "https://" ++ nid' ++ ".gomaestro-api.org/v1/policy/" ++ policyId' ++ "/assets?count=100"

      let apiKey = TE.encodeUtf8 . coerce . cpiMaestroToken . cfgCoreProvider $ coreCfg

      initialRequest <- parseRequest requestUrl
      let request = setRequestMethod "GET"
                  . addRequestHeader "accept" "application/json"
                  . addRequestHeader "api-key" apiKey
                  $ initialRequest

      response <- httpLBS request
      let body = getResponseBody response

      tokenNames <- case eitherDecode body of
        Left err  -> throwIO $ userError $ "Failed to decode JSON: " ++ err
        Right val -> do
          case parseMaybe extractAssetNames val of
            Nothing  -> throwIO $ userError "Could not extract asset names"
            Just tns -> pure $ unsafeTokenNameFromHex <$> tns

      let msgTokens = GYNonAdaToken policyId <$> tokenNames

      withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
        msgUtxos' <- forM msgTokens $ runGYTxQueryMonadIO nid providers . utxosWithAsset
        let msgUtxos = concat $ utxosToList <$> msgUtxos'

        putStr "\n"
        putStr "Client's messages on-chain:\n\n"

        mapM_ displayMsg $ utxoOutDatum <$> msgUtxos

        putStr "\n"

    _            -> throwIO $ userError "Only 'Maestro' is currently supported as provider."

------- :Helpers: -------

-- | Aeson parser to extract list of hex-encoded token names from the @.data[].asset_name@.
extractAssetNames :: Value -> Parser [T.Text]
extractAssetNames = withObject "root" $ \o -> do
  items <- o .: "data"
  mapM parseAssetName items
  where
    parseAssetName = withObject "item" $ \obj -> do
      tokenNameText <- obj .: "asset_name"
      pure $ tokenNameText

-- | Remove enclosing quotation marks
trimQuot :: String -> String
trimQuot = reverse . drop 1 . reverse . drop 1
