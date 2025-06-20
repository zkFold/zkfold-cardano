{-# LANGUAGE InstanceSigs #-}

module ZkFold.Cardano.Options.Common where

import           Cardano.Api                        (parseAddressAny)
import           Cardano.CLI.EraBased.Common.Option (parseFilePath, parseTxIn,
                                                     readerFromParsecParser)
import           Control.Exception                  (throwIO)
import qualified Data.ByteString.Base16             as B16
import qualified Data.ByteString.Char8              as BS
import           Data.String                        (fromString)
import           GeniusYield.GYConfig               as GY
import           GeniusYield.Types                  as GY
import           Options.Applicative                (Parser)
import qualified Options.Applicative                as Opt
import           Prelude

------------------------- :Alternatives: -------------------------

-- | Sum type for 'GYCoreConfig'.
data CoreConfigAlt = CoreConfigUseGY (Maybe GYCoreConfig)
                   | CoreConfigUseFile FilePath
                   deriving stock Show

-- | Sum type for 'GYPaymentSigningKey' input alternatives.
data SigningKeyAlt = SigningKeyUseGY GYPaymentSigningKey
                   | SigningKeyUseFile FilePath
                   deriving stock Show

fromCoreConfigAltIO :: CoreConfigAlt -> IO GYCoreConfig
fromCoreConfigAltIO cfg = case cfg of
  CoreConfigUseGY (Just gycfg) -> pure gycfg
  CoreConfigUseGY Nothing      -> throwIO $ userError "No core config specified."
  CoreConfigUseFile cfgfile    -> GY.coreConfigIO cfgfile

fromSigningKeyAltIO :: SigningKeyAlt -> IO GYPaymentSigningKey
fromSigningKeyAltIO sig = case sig of
  SigningKeyUseGY skey    -> pure skey
  SigningKeyUseFile sfile -> GY.readPaymentSigningKey sfile

pBenefOutAddress :: Parser GYAddress
pBenefOutAddress =
    Opt.option (readerFromParsecParser $ fmap GY.addressFromApi parseAddressAny) $
        mconcat
            [ Opt.long "beneficiary-address"
            , Opt.metavar "ADDRESS"
            , Opt.help "Address of beneficiary receiving token."
            ]

pGYCoreConfig :: Maybe GYCoreConfig -> Parser CoreConfigAlt
pGYCoreConfig mcfg = Opt.option (CoreConfigUseFile <$> Opt.maybeReader Just)
  ( Opt.long "core-config-file"
      <> Opt.value (CoreConfigUseGY mcfg)
      <> Opt.metavar "FILEPATH"
      <> Opt.help "Path to 'core config'.  This overrides the CORE_CONFIG_PATH environment variable.  The argument is optional if CORE_CONFIG_PATH is defined and mandatory otherwise."
      <> Opt.completer (Opt.bashCompleter "file")
  )

pTxInOref :: Parser GYTxOutRef
pTxInOref =
    Opt.option
        (txOutRefFromApi <$> readerFromParsecParser parseTxIn)
        ( Opt.long "tx-oref"
            <> Opt.metavar "TxId#TxIx"
            <> Opt.help "TxOutRef (reference to a Tx input) to be consumed at initialization."
        )

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

----- :parsing Message: -----

pMessageString :: Parser BS.ByteString
pMessageString = BS.pack <$> Opt.strOption
  ( Opt.long "message-text"
      <> Opt.metavar "ASCII"
      <> Opt.help "Asterizm message as string of ASCII characters."
  )

hexReader :: Opt.ReadM BS.ByteString
hexReader = Opt.eitherReader $ \s ->
  case B16.decode $ BS.pack s of
    Left err -> Left $ "Invalid hex string: " ++ err
    Right bs -> Right bs

pMessageHex :: Parser BS.ByteString
pMessageHex = Opt.option hexReader
    ( Opt.long "message-hex"
        <> Opt.metavar "HEX"
        <> Opt.help "Hex-encoded Asterizm message."
    )

pMessage :: Parser BS.ByteString
pMessage = Opt.asum [pMessageString, pMessageHex]

----- :parsing Message files: -----

pMessageFile :: Parser FilePath
pMessageFile = Opt.strOption
  ( Opt.long "message-file"
      <> Opt.value "message.private"
      <> Opt.showDefault
      <> Opt.metavar "FILEPATH"
      <> Opt.help "Path (relative to 'assets/') for PRIVATE file storing message."
      <> Opt.completer (Opt.bashCompleter "file")
  )

pMessageHashFile :: Parser FilePath
pMessageHashFile = Opt.strOption
  ( Opt.long "message-hash-file"
      <> Opt.value "message-hash.public"
      <> Opt.showDefault
      <> Opt.metavar "FILEPATH"
      <> Opt.help "Path (relative to 'assets/') for PUBLIC file storing message hash."
      <> Opt.completer (Opt.bashCompleter "file")
  )

----- :parsing PubKeyHash: -----

data AsterizmAgent = Client | Relayer

class HasPKHParser a where
  pkhParserFlag :: a -> String
  pkhParserHelp :: a -> String

  pPubKeyHash :: a -> Parser GYPubKeyHash
  pPubKeyHash x = fromString <$> Opt.strOption
    ( Opt.long (pkhParserFlag x)
        <> Opt.metavar "HEX"
        <> Opt.help (pkhParserHelp x)
    )

instance HasPKHParser AsterizmAgent where
  pkhParserFlag Client  = "client-pkh"
  pkhParserFlag Relayer = "relayer-pkh"

  pkhParserHelp Client  = "Hex-encoded client's pub-key-hash."
  pkhParserHelp Relayer = "Hex-encodec relayer's pub-key-hash."

----- :parsing OutFile: -----

data StageTx = AsterizmInit | AsterizmRelayer | AsterizmClient

class HasFileParser a where
  outFileName :: a -> String
  outFileFlag :: a -> String
  outFileHelp :: a -> String

  pOutFile :: a -> Parser FilePath
  pOutFile x = Opt.strOption
    ( Opt.long (outFileFlag x)
        <> Opt.value (outFileName x)
        <> Opt.metavar "FILEPATH"
        <> Opt.help (outFileHelp x)
        <> Opt.completer (Opt.bashCompleter "file")
    )

instance HasFileParser StageTx where
  outFileFlag AsterizmInit    = "asterizm-init-out-file"
  outFileFlag AsterizmRelayer = "asterizm-relayer-out-file"
  outFileFlag AsterizmClient  = "asterizm-client-out-file"

  outFileName AsterizmInit    = "asterizm-init.tx"
  outFileName AsterizmRelayer = "asterizm-relayer-mint.tx"
  outFileName AsterizmClient  = "asterizm-client-mint.tx"

  outFileHelp AsterizmInit    = "Path (relative to 'assets/') for Asterizm initialization tx out-file."
  outFileHelp AsterizmRelayer = "Path (relative to 'assets/') for Asterizm relayer-mint tx out-file."
  outFileHelp AsterizmClient  = "Path (relative to 'assets/') for Asterizm client-mint tx out-file."
