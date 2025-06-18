{-# LANGUAGE OverloadedStrings #-}

module ZkFold.Cardano.Asterizm.Transaction.Retrieve where

import           Control.Exception                  (throwIO)
import           Control.Monad                      (forM)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Coerce                        (coerce)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import           GeniusYield.GYConfig               (Confidential (..), GYCoreConfig (..), GYCoreProviderInfo (..),
                                                     withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Network.HTTP.Simple
import           Prelude
import           System.FilePath                    ((</>))

import           ZkFold.Cardano.Asterizm.Types      (fromAsterizmParams)
import           ZkFold.Cardano.Asterizm.Utils      (policyFromPlutus)
import           ZkFold.Cardano.Options.Common
import           ZkFold.Cardano.UPLC.AsterizmClient (asterizmClientCompiled)


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

displayMsg :: GYOutDatum -> IO ()
displayMsg od = case od of
  GYOutDatumInline d -> print $ datumToPlutus' d
  _                  -> putStrLn "Unexpected: no datum found."

retrieveMsgs :: Transaction -> IO ()
retrieveMsgs (Transaction path coreCfg') = do
  let assetsPath = path </> "assets"
      setupFile  = assetsPath </> "asterizm-setup.json"

  mAsterizmParams <- decodeFileStrict setupFile

  asterizmSetup <- case mAsterizmParams of
    Just ap -> pure $ fromAsterizmParams ap
    Nothing -> throwIO $ userError "Unable to decode Asterizm setup file."

  let policyId  = snd . policyFromPlutus . asterizmClientCompiled $ asterizmSetup
  let policyId' = trimQuot $ show policyId

  coreCfg <- fromCoreConfigAltIO coreCfg'

  let nid = cfgNetworkId coreCfg
  nidStg <- fromNetworkIdIO nid

  let requestUrl = "https://" ++ nidStg ++ ".gomaestro-api.org/v1/policy/" ++ policyId' ++ "/assets?count=100"

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
    putStr"Messages on-chain:\n\n"
    mapM_ displayMsg $ utxoOutDatum <$> msgUtxos
    putStr "\n"

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
