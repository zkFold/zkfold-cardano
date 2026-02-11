{-# LANGUAGE OverloadedStrings #-}

module ZkFold.Cardano.Asterizm.Transaction.Retrieve where

import           Control.Exception             (throwIO)
import           Control.Monad                 (forM)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Coerce                   (coerce)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           GeniusYield.GYConfig          (Confidential (..), GYCoreConfig (..), GYCoreProviderInfo (..),
                                                withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Network.HTTP.Simple
import           Prelude
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (AsterizmSetup (..))
import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import           ZkFold.Cardano.CLI.Parsers    (CoreConfigAlt, fromCoreConfigAltIO)
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmClientCompiled)

-- | Convert AsterizmSetup to policy id for querying
setupToPolicyId :: AsterizmSetup -> GYMintingPolicyId
setupToPolicyId AsterizmSetup{..} = snd . policyFromPlutus $
  asterizmClientCompiled (pubKeyHashToPlutus acsClientPKH)
                         (mintingPolicyIdToCurrencySymbol <$> acsAllowedRelayers)


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

displayMsg :: GYOutDatum -> IO ()
displayMsg od = case od of
  GYOutDatumInline d -> print $ datumToPlutus' d
  _                  -> putStrLn "Unexpected: no datum found."

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
