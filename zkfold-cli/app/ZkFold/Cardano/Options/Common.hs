module ZkFold.Cardano.Options.Common where

import           Cardano.Api                         (AddressAny, TxIn, parseAddressAny)
import           Cardano.CLI.EraBased.Options.Common (parseFilePath, parseTxIn, readerFromParsecParser)
import           Control.Exception                   (throwIO)
import           Control.Monad                       (when)
import qualified Data.ByteString.Char8               as BS
import           Data.Aeson                          (decodeFileStrict, encodeFile)
import           Data.Maybe                          (fromJust, isJust)
import           Data.String                         (fromString)
import           GeniusYield.GYConfig                as GY
import           GeniusYield.Types                   as GY
import           Options.Applicative                 (Parser)
import qualified Options.Applicative                 as Opt
import           Prelude
import           System.FilePath                     ((</>))
import qualified System.IO                           as IO


--------------------------- :Defaults: ---------------------------

-- | Default tag (arbitrary integer) for 'ForwardingMint' script.
defaultForwardingMintTag :: Integer
defaultForwardingMintTag = 43

------------------------- :Alternatives: -------------------------

-- | Sum type for 'GYCoreConfig'
data CoreConfigAlt = CoreConfigUseGY (Maybe GYCoreConfig)
                   | CoreConfigUseFile FilePath
                   deriving stock Show

-- | Sum type for 'GYMintingPolicyId' input alternatives
data PolicyIdAlt = PolicyIdUseGY GYMintingPolicyId
                 | PolicyIdUseFile FilePath

-- | Sum type for 'GYPaymentSigningKey' input alternatives
data SigningKeyAlt = SigningKeyUseGY GYPaymentSigningKey
                   | SigningKeyUseFile FilePath
                   deriving stock Show

-- | Sum type for 'TxId' input alternatives
data TxIdAlt = TxIdUseGY GYTxId
             | TxIdUseFile FilePath
             deriving stock Show

fromCoreConfigAltIO :: CoreConfigAlt -> IO GYCoreConfig
fromCoreConfigAltIO cfg = case cfg of
  CoreConfigUseGY (Just gycfg) -> pure gycfg
  CoreConfigUseGY Nothing      -> throwIO $ userError "No core config specified."
  CoreConfigUseFile cfgfile    -> GY.coreConfigIO cfgfile

fromPolicyIdAltIO :: FilePath -> PolicyIdAlt -> IO GYMintingPolicyId
fromPolicyIdAltIO path pid = case pid of
  PolicyIdUseGY gypid    -> pure gypid
  PolicyIdUseFile gyfile -> fromString <$> IO.readFile (path </> gyfile)

fromSigningKeyAltIO :: SigningKeyAlt -> IO GYPaymentSigningKey
fromSigningKeyAltIO sig = case sig of
  SigningKeyUseGY skey    -> pure skey
  SigningKeyUseFile sfile -> GY.readPaymentSigningKey sfile

fromTxIdAltIO :: TxIdAlt -> IO GYTxId
fromTxIdAltIO tid = case tid of
  TxIdUseGY txId     -> pure txId
  TxIdUseFile txFile -> decodeFileStrict txFile >>= pure . fromJust

----------------------------- :Tx: -------------------------------

data SubmittedTx = SubmittedTx
  { stTxId  :: !GYTxId
  , stTxFee :: !(Maybe Integer)
  } deriving stock Show

wrapUpSubmittedTx :: FilePath -> SubmittedTx -> IO ()
wrapUpSubmittedTx outFile SubmittedTx{..} = do
  putStr "\n"
  
  when (isJust stTxFee) $
    putStr $ "Estimated transaction fee: " ++ (show $ fromJust stTxFee) ++ " Lovelace\n"

  putStr $ "Transaction Id: " ++ show stTxId ++ "\n\n"

  encodeFile outFile stTxId

--------------------------- :Parsers: ----------------------------

pChangeAddress :: Parser AddressAny
pChangeAddress =
  Opt.option (readerFromParsecParser parseAddressAny) $
      mconcat
          [ Opt.long "change-address"
          , Opt.metavar "ADDRESS"
          , Opt.help "Address where ADA in excess of the tx fee will go to."
          ]

pChangeAddress' :: Parser GYAddress
pChangeAddress' =
  Opt.option (readerFromParsecParser $ fmap GY.addressFromApi parseAddressAny) $
      mconcat
          [ Opt.long "change-address"
          , Opt.metavar "ADDRESS"
          , Opt.help "Address where ADA in excess of the tx fee will go to."
          ]

pOutAddress :: Parser AddressAny
pOutAddress =
    Opt.option (readerFromParsecParser parseAddressAny) $
        mconcat
            [ Opt.long "out-address"
            , Opt.metavar "ADDRESS"
            , Opt.help "Tx out address."
            ]

pOutAddress' :: Parser GYAddress
pOutAddress' =
    Opt.option (readerFromParsecParser $ fmap GY.addressFromApi parseAddressAny) $
        mconcat
            [ Opt.long "out-address"
            , Opt.metavar "ADDRESS"
            , Opt.help "Tx out address."
            ]

pGYCoreConfig' :: Maybe GYCoreConfig -> Parser CoreConfigAlt
pGYCoreConfig' mcfg = Opt.option (CoreConfigUseFile <$> Opt.maybeReader Just)
  ( Opt.long "core-config-file"
      <> Opt.value (CoreConfigUseGY mcfg)
      <> Opt.metavar "FILEPATH"
      <> Opt.help "Path to 'core config'.  This overrides the CORE_CONFIG_PATH environment variable.  The argument is optional if CORE_CONFIG_PATH is defined and mandatory otherwise."
      <> Opt.completer (Opt.bashCompleter "file")
  )

pGYCoreConfig :: Parser FilePath
pGYCoreConfig = parseFilePath "path-to-gycoreconfig" "Path to GYCoreConfig."

pTxInOnly :: Parser TxIn
pTxInOnly =
    Opt.option
        (readerFromParsecParser parseTxIn)
        ( Opt.long "tx-in"
            <> Opt.metavar "TX-IN"
            <> Opt.help "TxId#TxIx"
        )

pOutFile :: Parser FilePath
pOutFile = parseFilePath "tx-out-file" "Tx out address."

pFMTag :: Parser Integer
pFMTag = Opt.option Opt.auto
  ( Opt.long "forwarding-mint-tag"
      <> Opt.value defaultForwardingMintTag 
      <> Opt.metavar "INTEGER"
      <> Opt.help "Tag (integer) discerning 'forwarding-mint' script."
  )

pReward :: Parser GYValue
pReward = GY.valueFromLovelace <$> Opt.option Opt.auto
  ( Opt.long "lovelace-reward"
      <> Opt.metavar "INTEGER"
      <> Opt.help "Reward lovelace value."
  )

----- :parsing PolicyId: -----

pPolicyId :: Parser GYMintingPolicyId
pPolicyId = fromString <$> Opt.strOption
  ( Opt.long "policy-id"
      <> Opt.metavar "HEX"
      <> Opt.help "Hex-encoded policy id."
  )

pPolicyIdFile :: Parser FilePath
pPolicyIdFile = parseFilePath "policy-id-file" "Policy ID file."

pPolicyIdAlt :: Parser PolicyIdAlt
pPolicyIdAlt = fmap (maybe defaultPolicyId id) . Opt.optional $ Opt.asum
    [ PolicyIdUseGY <$> pPolicyId
    , PolicyIdUseFile <$> pPolicyIdFile
    ]
  where
    defaultPolicyId = PolicyIdUseFile "plonkupVerifierTokenPolicyId.txt"

----- :parsing GYPaymentSigningKey: -----

pSigningKey :: Parser GYPaymentSigningKey
pSigningKey = Opt.option (Opt.eitherReader parseKey)
    ( Opt.long "signing-key"
        <> Opt.metavar "HEX"
        <> Opt.help "Hex-encoded signing key"
    )
  where
    parseKey :: String -> Either String GYPaymentSigningKey
    parseKey str = GY.signingKeyFromRawBytesHex (BS.pack str)

pSigningKeyFile :: Parser FilePath
pSigningKeyFile = parseFilePath "signing-key-file" "Payment signing key file."

pSigningKeyAlt :: Parser SigningKeyAlt
pSigningKeyAlt = Opt.asum
  [ SigningKeyUseGY <$> pSigningKey
  , SigningKeyUseFile <$> pSigningKeyFile
  ]

----- :parsing GYTxId: -----

pTxId :: Parser GYTxId
pTxId = Opt.option (Opt.eitherReader GY.txIdFromHexE)
    ( Opt.long "tx-id"
        <> Opt.metavar "HEX"
        <> Opt.help "Hex-encoded TxId."
    )

pTxIdFile :: Parser FilePath
pTxIdFile = parseFilePath "tx-id-file" "Tx id address."

pTxIdAlt :: Parser TxIdAlt
pTxIdAlt = Opt.asum
  [ TxIdUseGY <$> pTxId
  , TxIdUseFile <$> pTxIdFile
  ]
