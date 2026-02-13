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

import           ZkFold.Cardano.Asterizm.Types      (MessageDirection (..))

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

----- :read ByteString: -----

hexReader :: Opt.ReadM BS.ByteString
hexReader = Opt.eitherReader $ \s ->
  case B16.decode $ BS.pack s of
    Left err -> Left $ "Invalid hex string: " ++ err
    Right bs -> Right bs

----- :parsing Message: -----

pMessageHash :: Parser BS.ByteString
pMessageHash = Opt.option hexReader
    ( Opt.long "message-hash"
        <> Opt.metavar "HEX"
        <> Opt.help "Hex-encoded Asterizm message hash (32 bytes)."
    )

pMessage :: Parser BS.ByteString
pMessage = Opt.option hexReader
    ( Opt.long "message"
        <> Opt.metavar "HEX"
        <> Opt.help "Hex-encoded Asterizm structured message."
    )

----- :parsing MessageDirection: -----

pMessageDirection :: Parser MessageDirection
pMessageDirection = Opt.asum
    [ Opt.flag' Incoming
        ( Opt.long "incoming"
            <> Opt.help "Incoming cross-chain message (requires relayer verification)."
        )
    , Opt.flag' Outgoing
        ( Opt.long "outgoing"
            <> Opt.help "Outgoing cross-chain message (no relayer verification needed)."
        )
    ]

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
