module ZkFold.Cardano.Options.Common where

import           Cardano.Api                        (parseAddressAny)
import           Cardano.CLI.EraBased.Common.Option (parseFilePath, readerFromParsecParser)
import           Data.Aeson                         (decodeFileStrict)
import           Data.Maybe                         (fromJust)
import           Data.String                        (fromString)
import           GeniusYield.Types                  as GY
import           Options.Applicative                (Parser)
import qualified Options.Applicative                as Opt
import           Prelude
import           System.FilePath                    ((</>))
import qualified System.IO                          as IO

--------------------------- :Defaults: ---------------------------

-- | Default tag (arbitrary integer) for 'ForwardingMint' script.
defaultForwardingMintTag :: Integer
defaultForwardingMintTag = 2927

-- | Name of data tokens mint Tx out-file.
dataOut :: String
dataOut = "dataTokens.tx"

------------------------- :Alternatives: -------------------------

-- | Sum type for 'GYMintingPolicyId' input alternatives.
data PolicyIdAlt = PolicyIdUseGY GYMintingPolicyId
                 | PolicyIdUseFile FilePath
                 deriving stock Show

-- | Sum type for 'GYAssetClass' input alternatives.
data TokenAlt = TokenUseGY GYAssetClass
              | TokenUseFile FilePath
              deriving stock Show

-- | Sum type for 'TxId' input alternatives.
data TxIdAlt = TxIdUseGY GYTxId
             | TxIdUseFile FilePath
             deriving stock Show

fromPolicyIdAltIO :: FilePath -> PolicyIdAlt -> IO GYMintingPolicyId
fromPolicyIdAltIO path pid = case pid of
  PolicyIdUseGY gypid    -> pure gypid
  PolicyIdUseFile gyfile -> fromString <$> IO.readFile (path </> gyfile)

fromTokenAltIO :: FilePath -> TokenAlt -> IO GYAssetClass
fromTokenAltIO path tok = case tok of
  TokenUseGY gytoken  -> pure gytoken
  TokenUseFile gyfile -> decodeFileStrict (path </> gyfile) >>= pure . fromJust

fromTxIdAltIO :: FilePath -> TxIdAlt -> IO GYTxId
fromTxIdAltIO path tid = case tid of
  TxIdUseGY txId     -> pure txId
  TxIdUseFile txFile -> decodeFileStrict (path </> txFile) >>= pure . fromJust

--------------------------- :Parsers: ----------------------------

pFMTag :: Parser Integer
pFMTag = Opt.option Opt.auto
  ( Opt.long "forwarding-mint-tag"
      <> Opt.value defaultForwardingMintTag
      <> Opt.metavar "INTEGER"
      <> Opt.help "Tag (integer) discerning 'forwarding-mint' script."
  )

pParkOutAddress :: Parser GYAddress
pParkOutAddress =
    Opt.option (readerFromParsecParser $ fmap GY.addressFromApi parseAddressAny) $
        mconcat
            [ Opt.long "parking-address"
            , Opt.metavar "ADDRESS"
            , Opt.help "Address to park scripts at."
            ]

----- :parsing PolicyId: -----

pPolicyId :: Parser GYMintingPolicyId
pPolicyId = fromString <$> Opt.strOption
  ( Opt.long "policy-id"
      <> Opt.metavar "HEX"
      <> Opt.help "Hex-encoded policy id."
  )

pPolicyIdFile :: Parser FilePath
pPolicyIdFile = parseFilePath "policy-id-file" "Path (relative to 'assets/') for Policy ID file."

pPolicyIdAlt :: Parser PolicyIdAlt
pPolicyIdAlt = fmap (maybe defaultPolicyId id) . Opt.optional $ Opt.asum
    [ PolicyIdUseGY <$> pPolicyId
    , PolicyIdUseFile <$> pPolicyIdFile
    ]
  where
    defaultPolicyId = PolicyIdUseFile "plonkupVerifierTokenPolicyId.txt"

----- :parsing Token: -----

pToken :: Parser GYAssetClass
pToken = fromString <$> Opt.strOption
  ( Opt.long "burn-token"
      <> Opt.metavar "POLICYID.TOKENNAME"
      <> Opt.help "Token to burn."
  )

pTokenFile :: Parser FilePath
pTokenFile = parseFilePath "burn-token-file" "Path (relative to 'assets/') for token-to-burn file."

pTokenAlt :: Parser TokenAlt
pTokenAlt = fmap (maybe defaultToken id) . Opt.optional $ Opt.asum
    [ TokenUseGY <$> pToken
    , TokenUseFile <$> pTokenFile
    ]
  where
    defaultToken = TokenUseFile "lastMintedToken.txt"

----- :parsing GYTxId: -----

pTxId :: Parser GYTxId
pTxId = Opt.option (Opt.eitherReader GY.txIdFromHexE)
    ( Opt.long "tx-id"
        <> Opt.metavar "HEX"
        <> Opt.help "Hex-encoded TxId of setup (initialization)."
    )

pTxIdFile :: Parser FilePath
pTxIdFile = parseFilePath "tx-id-file" "Path (relative to 'assets/') of setup TxId file."

pTxIdAlt :: Parser TxIdAlt
pTxIdAlt = Opt.asum
  [ TxIdUseGY <$> pTxId
  , TxIdUseFile <$> pTxIdFile
  ]

----- :parsing OutFile: -----

data StageTx = TokenInit | TokenTransfer | TokenMinting | TokenBurning

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
  outFileFlag TokenInit     = "init-out-file"
  outFileFlag TokenTransfer = "transfer-out-file"
  outFileFlag TokenMinting  = "mint-out-file"
  outFileFlag TokenBurning  = "burn-out-file"

  outFileName TokenInit     = "token-init.tx"
  outFileName TokenTransfer = "token-transfer.tx"
  outFileName TokenMinting  = "token-mint.tx"
  outFileName TokenBurning  = "token-burn.tx"

  outFileHelp TokenInit     = "Path (relative to 'assets/') for plonkupVerifierToken initialization tx out-file."
  outFileHelp TokenTransfer = "Path (relative to 'assets/') for plonkupVerifierToken reward transfer tx out-file."
  outFileHelp TokenMinting  = "Path (relative to 'assets/') for plonkupVerifierToken minting tx out-file."
  outFileHelp TokenBurning  = "Path (relative to 'assets/') for plonkupVerifierToken burning tx out-file."
