module ZkFold.Cardano.Options.Common where

import qualified Cardano.Api                        as Api
import           Cardano.CLI.EraBased.Common.Option (parseFilePath)
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
readPaymentVerificationKey _ = undefined

fromPubKeyHashAltIO :: PubKeyHashAlt -> IO GYPubKeyHash
fromPubKeyHashAltIO _ = undefined

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
  outFileFlag AsterizmInit    = "init-out-file"
  outFileFlag AsterizmRelayer = "relayer-out-file"
  outFileFlag AsterizmClient  = "client-out-file"

  outFileName AsterizmInit    = "asterizm-init.tx"
  outFileName AsterizmRelayer = "asterizm-relayer-mint.tx"
  outFileName AsterizmClient  = "asterizm-client-mint.tx"

  outFileHelp AsterizmInit    = "Path (relative to 'assets/') for Asterizm initialization tx out-file."
  outFileHelp AsterizmRelayer = "Path (relative to 'assets/') for Asterizm relayer-mint tx out-file."
  outFileHelp AsterizmClient  = "Path (relative to 'assets/') for Asterizm client-mint tx out-file."
