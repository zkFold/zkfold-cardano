module ZkFold.Cardano.Options.Common where

import qualified Cardano.Api                        as Api
import           Cardano.CLI.EraBased.Common.Option (parseFilePath, readerFromParsecParser)
import           Control.Exception                  (throwIO)
import qualified Data.ByteString.Base16             as B16
import qualified Data.ByteString.Char8              as BS
import           Data.String                        (fromString)
import           GeniusYield.Types                  as GY
import           Options.Applicative                (Parser)
import qualified Options.Applicative                as Opt
import           Prelude

----- :Alternatives: -----

-- | Sum type for 'GYPubKeyHash' input alternatives.
data PubKeyHashAlt = PubKeyHashUseGY GYPubKeyHash
                   | PaymentVerKeyUseFile FilePath
                   deriving stock Show

readPaymentVerificationKey :: FilePath -> IO GYPaymentVerificationKey
readPaymentVerificationKey fp = do
  s <- Api.readFileTextEnvelope (Api.AsVerificationKey Api.AsPaymentKey) (Api.File fp)
  case s of
    Left err  -> throwIO $ userError (show err)
    Right vpk -> return $ paymentVerificationKeyFromApi vpk

fromPubKeyHashAltIO :: PubKeyHashAlt -> IO GYPubKeyHash
fromPubKeyHashAltIO pkha = case pkha of
  PubKeyHashUseGY pkh     -> pure pkh
  PaymentVerKeyUseFile fp -> do
    vkey <- readPaymentVerificationKey fp
    return $ pubKeyHash vkey

----- :parsing Registry Address: -----

pRegistryAddress :: Parser GYAddress
pRegistryAddress =
    Opt.option (readerFromParsecParser $ fmap GY.addressFromApi Api.parseAddressAny) $
        mconcat
            [ Opt.long "registry-address"
            , Opt.metavar "ADDRESS"
            , Opt.help "Address to park relayer's registry at."
            ]

----- :read ByteString: -----

hexReader :: Opt.ReadM BS.ByteString
hexReader = Opt.eitherReader $ \s ->
  case B16.decode $ BS.pack s of
    Left err -> Left $ "Invalid hex string: " ++ err
    Right bs -> Right bs

----- :parsing Message: -----

pMessageString :: Parser BS.ByteString
pMessageString = BS.pack <$> Opt.strOption
  ( Opt.long "message-text"
      <> Opt.metavar "ASCII"
      <> Opt.help "Asterizm message as string of ASCII characters."
  )

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
  vkfParserFlag :: a -> String
  vkfParserHelp :: a -> String

  pPubKeyHash :: a -> Parser GYPubKeyHash
  pPubKeyHash x = fromString <$> Opt.strOption
    ( Opt.long (pkhParserFlag x)
        <> Opt.metavar "HEX"
        <> Opt.help (pkhParserHelp x)
    )

  pVerificationKeyFile :: a -> Parser FilePath
  pVerificationKeyFile x = parseFilePath (vkfParserFlag x) (vkfParserHelp x)

  pPubKeyHashAlt :: a -> Parser PubKeyHashAlt
  pPubKeyHashAlt x = Opt.asum
    [ PubKeyHashUseGY <$> pPubKeyHash x
    , PaymentVerKeyUseFile <$> pVerificationKeyFile x
    ]

instance HasPKHParser AsterizmAgent where
  pkhParserFlag Client  = "client-pkh"
  pkhParserFlag Relayer = "relayer-pkh"
  vkfParserFlag Client  = "client-vkey-file"
  vkfParserFlag Relayer = "relayer-vkey-file"

  pkhParserHelp Client  = "Hex-encoded client's pub-key-hash."
  pkhParserHelp Relayer = "Hex-encodec relayer's pub-key-hash."
  vkfParserHelp Client  = "Client's payment verification-key file."
  vkfParserHelp Relayer = "Relayer's payment verification-key file."

----- :parsing AsterizmTransferMeta : -----

data AsterizmTransferFieldBS = ATFSrcAddress | ATFDstAddress | ATFTxId | ATFTransferHash
data AsterizmChainId = ACISrc | ACIDst

class HasBSParser a where
  bsParserFlag :: a -> String
  bsParserHelp :: a -> String

  pTransferFieldBS :: a -> Parser BS.ByteString
  pTransferFieldBS x = Opt.option hexReader
    ( Opt.long (bsParserFlag x)
        <> Opt.metavar "HEX"
        <> Opt.help (bsParserHelp x)
    )

instance HasBSParser AsterizmTransferFieldBS where
  bsParserFlag ATFSrcAddress   = "init-src-address"
  bsParserFlag ATFDstAddress   = "init-dst-address"
  bsParserFlag ATFTxId         = "init-tx-id"
  bsParserFlag ATFTransferHash = "init-transfer-hash"

  bsParserHelp ATFSrcAddress   = "Hex-encoded source address."
  bsParserHelp ATFDstAddress   = "Hex-encoded destination address."
  bsParserHelp ATFTxId         = "Hex-encoded TxId."
  bsParserHelp ATFTransferHash = "Hex-encoded transfer hash."

class HasChainIdParser a where
  chainIdFlag :: a -> String
  chainIdHelp :: a -> String

  pTransferChainId :: a -> Parser Integer
  pTransferChainId x = Opt.option Opt.auto
    ( Opt.long (chainIdFlag x)
        <> Opt.metavar "INTEGER"
        <> Opt.help (chainIdHelp x)
    )

instance HasChainIdParser AsterizmChainId where
  chainIdFlag ACISrc = "init-src-chain-id"
  chainIdFlag ACIDst = "init-dst-chain-id"

  chainIdHelp ACISrc = "Source chain id (integer)."
  chainIdHelp ACIDst = "Destination chain id (integer)."

pTransferNotifyFlag :: Parser Bool
pTransferNotifyFlag =
  Opt.option Opt.auto
      ( Opt.long "init-notify-flag"
          <> Opt.metavar "BOOL"
          <> Opt.help "Whether to notify (bool)."
      )

----- :parsing OutFile: -----

data StageTx = AsterizmInit | AsterizmRelayer | AsterizmClient | AsterizmInitRelay

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
  outFileFlag AsterizmInit      = "init-out-file"
  outFileFlag AsterizmRelayer   = "relayer-out-file"
  outFileFlag AsterizmClient    = "client-out-file"
  outFileFlag AsterizmInitRelay = "init-relay-out-file"

  outFileName AsterizmInit      = "asterizm-init.tx"
  outFileName AsterizmRelayer   = "asterizm-relayer-mint.tx"
  outFileName AsterizmClient    = "asterizm-client-mint.tx"
  outFileName AsterizmInitRelay = "asterizm-init-relay.tx"

  outFileHelp AsterizmInit      = "Path (relative to 'assets/') for Asterizm initialization tx out-file."
  outFileHelp AsterizmRelayer   = "Path (relative to 'assets/') for Asterizm relayer-mint tx out-file."
  outFileHelp AsterizmClient    = "Path (relative to 'assets/') for Asterizm client-mint tx out-file."
  outFileHelp AsterizmInitRelay = "Path (relative to 'assets/') for Asterizm init-relay tx out-file."
