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
import           Data.Maybe                    (mapMaybe)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           GeniusYield.GYConfig          (Confidential (..), GYCoreConfig (..), GYCoreProviderInfo (..),
                                                coreConfigIO, withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Network.HTTP.Simple
import           PlutusLedgerApi.V3            (BuiltinByteString, CurrencySymbol (..), fromBuiltin, toBuiltin)
import           PlutusTx                      (unsafeFromBuiltinData)
import           Prelude

import           ZkFold.Cardano.Asterizm.Types (AsterizmMessage (..), MessageDirection (..), fromByteString)
import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import           ZkFold.Cardano.Options.Common (readPaymentVerificationKey)
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmClientCompiled, asterizmRelayerCompiled, asterizmUserCompiled)


data Transaction = Transaction
  { coreCfgFile      :: !FilePath
  , clientVKeyFile   :: !FilePath
  , relayerVKeyFiles :: ![FilePath]
  , trustedAddresses :: ![BS.ByteString]
  , direction        :: !MessageDirection
  }

-- | Derive the unified client policy ID from verification keys and trusted addresses.
derivePolicyId :: GYPaymentVerificationKey -> [GYPaymentVerificationKey] -> [BS.ByteString] -> GYMintingPolicyId
derivePolicyId clientVkey relayerVkeys trustedAddressBSs =
  let clientPKH = pubKeyHashToPlutus $ pubKeyHash clientVkey
      relayerCSs = fmap (mintingPolicyIdToCurrencySymbol . snd . policyFromPlutus . asterizmRelayerCompiled . pubKeyHashToPlutus . pubKeyHash) relayerVkeys
      userCS = mintingPolicyIdToCurrencySymbol . snd . policyFromPlutus $ asterizmUserCompiled
      trustedAddresses = toBuiltin <$> trustedAddressBSs
  in snd . policyFromPlutus $ asterizmClientCompiled clientPKH relayerCSs userCS trustedAddresses

fromNetworkIdIO :: GYNetworkId -> IO String
fromNetworkIdIO nid = case nid of
  GYMainnet        -> pure "mainnet"
  GYTestnetPreprod -> pure "preprod"
  GYTestnetPreview -> pure "preview"
  _                -> throwIO $ userError "Network not supported."

-- | Decode a structured Asterizm message from datum.
messageFromDatum :: GYOutDatum -> Maybe AsterizmMessage
messageFromDatum od = case od of
  GYOutDatumInline d -> do
    let plutusDatum = datumToPlutus' d
        rawBytes = fromBuiltin (unsafeFromBuiltinData plutusDatum :: BuiltinByteString)
    fromByteString rawBytes
  _ -> Nothing

-- | Display a structured Asterizm message.
displayMsg :: AsterizmMessage -> IO ()
displayMsg msg = do
  putStrLn $ "  Source Chain ID: " ++ show (amSrcChainId msg)
  putStrLn $ "  Source Address:  " ++ bsToHexStr (amSrcAddress msg)
  putStrLn $ "  Dest Chain ID:   " ++ show (amDstChainId msg)
  putStrLn $ "  Dest Address:    " ++ bsToHexStr (amDstAddress msg)
  putStrLn $ "  Tx ID:           " ++ bsToHexStr (amTxId msg)
  putStrLn $ "  Payload (hex):   " ++ bsToHexStr (amPayload msg)
  putStrLn $ "  Payload (ASCII): " ++ bsToAscii (amPayload msg)
  putStrLn ""

-- | Convert ByteString to hex string
bsToHexStr :: BS.ByteString -> String
bsToHexStr = T.unpack . TE.decodeUtf8 . B16.encode

-- | Convert ByteString to ASCII, replacing non-printable chars with '.'
bsToAscii :: BS.ByteString -> String
bsToAscii = map (\c -> if isPrint c then c else '.') . map (toEnum . fromIntegral) . BS.unpack

clientPolicyAddress :: GYMintingPolicyId -> BS.ByteString
clientPolicyAddress = leftPad32 . fromBuiltin . unCurrencySymbol . mintingPolicyIdToCurrencySymbol

leftPad32 :: BS.ByteString -> BS.ByteString
leftPad32 bs = BS.replicate (32 - BS.length bs) 0 <> bs

matchesDirection :: BS.ByteString -> MessageDirection -> AsterizmMessage -> Bool
matchesDirection clientAddress dir msg = case dir of
  Incoming -> amDstAddress msg == clientAddress
  Outgoing -> amSrcAddress msg == clientAddress

retrieveMsgs :: Transaction -> IO ()
retrieveMsgs (Transaction cfgFile clientVkeyFile relayerVkeyFiles trustedAddressBSs dir) = do
  coreCfg      <- coreConfigIO cfgFile
  clientVkey   <- readPaymentVerificationKey clientVkeyFile
  relayerVkeys <- mapM readPaymentVerificationKey relayerVkeyFiles

  case cfgCoreProvider coreCfg of
    GYMaestro {} -> do
      let policyId  = derivePolicyId clientVkey relayerVkeys trustedAddressBSs
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
      let clientAddress = clientPolicyAddress policyId

      withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
        msgUtxos' <- forM msgTokens $ runGYTxQueryMonadIO nid providers . utxosWithAsset
        let msgUtxos = concat $ utxosToList <$> msgUtxos'
            msgs = filter (matchesDirection clientAddress dir) $ mapMaybe (messageFromDatum . utxoOutDatum) msgUtxos

        let dirLabel = case dir of
              Incoming -> "incoming"
              Outgoing -> "outgoing"

        putStr "\n"
        putStr $ "Client's " ++ dirLabel ++ " messages on-chain:\n\n"

        mapM_ displayMsg msgs

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
