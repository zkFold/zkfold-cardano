{-# LANGUAGE InstanceSigs #-}

module ZkFold.Cardano.Options.Common where

import           Cardano.Api                        (AddressAny, TxIn, parseAddressAny)
import           Cardano.CLI.EraBased.Common.Option (pScriptRedeemerOrFile, parseFilePath, parseTxIn,
                                                     readerFromParsecParser)
import           Cardano.CLI.Type.Common            (ScriptDataOrFile)
import           Control.Exception                  (throwIO)
import           Data.Aeson                         (decodeFileStrict)
import qualified Data.ByteString.Base16             as B16
import qualified Data.ByteString.Char8              as BS
import           Data.Maybe                         (fromJust)
import           Data.String                        (fromString)
import           GeniusYield.GYConfig               as GY
import           GeniusYield.Types                  as GY
import           Options.Applicative                (Parser, (<|>))
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

-- | Sum type for 'GYCoreConfig'.
data CoreConfigAlt = CoreConfigUseGY (Maybe GYCoreConfig)
                   | CoreConfigUseFile FilePath
                   deriving stock Show

-- | Sum type for 'GYMintingPolicyId' input alternatives.
data PolicyIdAlt = PolicyIdUseGY GYMintingPolicyId
                 | PolicyIdUseFile FilePath
                 deriving stock Show

-- | Sum type for 'GYPaymentSigningKey' input alternatives.
data SigningKeyAlt = SigningKeyUseGY GYPaymentSigningKey
                   | SigningKeyUseFile FilePath
                   deriving stock Show

-- | Sum type for 'GYAssetClass' input alternatives.
data TokenAlt = TokenUseGY GYAssetClass
              | TokenUseFile FilePath
              deriving stock Show

-- | Sum type for 'TxId' input alternatives.
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

fromTokenAltIO :: FilePath -> TokenAlt -> IO GYAssetClass
fromTokenAltIO path tok = case tok of
  TokenUseGY gytoken  -> pure gytoken
  TokenUseFile gyfile -> decodeFileStrict (path </> gyfile) >>= pure . fromJust

fromTxIdAltIO :: FilePath -> TxIdAlt -> IO GYTxId
fromTxIdAltIO path tid = case tid of
  TxIdUseGY txId     -> pure txId
  TxIdUseFile txFile -> decodeFileStrict (path </> txFile) >>= pure . fromJust

--------------------------- :Parsers: ----------------------------

pChangeAddress :: Parser GYAddress
pChangeAddress =
  Opt.option (readerFromParsecParser $ fmap GY.addressFromApi parseAddressAny) $
      mconcat
          [ Opt.long "change-address"
          , Opt.metavar "ADDRESS"
          , Opt.help "Address where ADA in excess of the tx fee will go to."
          ]

pFMTag :: Parser Integer
pFMTag = Opt.option Opt.auto
  ( Opt.long "forwarding-mint-tag"
      <> Opt.value defaultForwardingMintTag
      <> Opt.metavar "INTEGER"
      <> Opt.help "Tag (integer) discerning 'forwarding-mint' script."
  )

pOutAddress :: Parser AddressAny
pOutAddress =
    Opt.option (readerFromParsecParser parseAddressAny) $
        mconcat
            [ Opt.long "out-address"
            , Opt.metavar "ADDRESS"
            , Opt.help "Tx out address."
            ]

pParkOutAddress :: Parser GYAddress
pParkOutAddress =
    Opt.option (readerFromParsecParser $ fmap GY.addressFromApi parseAddressAny) $
        mconcat
            [ Opt.long "parking-address"
            , Opt.metavar "ADDRESS"
            , Opt.help "Address to park scripts at."
            ]

pBenefOutAddress :: Parser GYAddress
pBenefOutAddress =
    Opt.option (readerFromParsecParser $ fmap GY.addressFromApi parseAddressAny) $
        mconcat
            [ Opt.long "beneficiary-address"
            , Opt.metavar "ADDRESS"
            , Opt.help "Address of beneficiary receiving tokens."
            ]

pFeeAddress :: Parser GYAddress
pFeeAddress =
    Opt.option (readerFromParsecParser $ fmap GY.addressFromApi parseAddressAny) $
        mconcat
            [ Opt.long "fee-address"
            , Opt.metavar "ADDRESS"
            , Opt.help "Tx out address."
            ]

pGYCoreConfig :: Maybe GYCoreConfig -> Parser CoreConfigAlt
pGYCoreConfig mcfg = Opt.option (CoreConfigUseFile <$> Opt.maybeReader Just)
  ( Opt.long "core-config-file"
      <> Opt.value (CoreConfigUseGY mcfg)
      <> Opt.metavar "FILEPATH"
      <> Opt.help "Path to 'core config'.  This overrides the CORE_CONFIG_PATH environment variable.  The argument is optional if CORE_CONFIG_PATH is defined and mandatory otherwise."
      <> Opt.completer (Opt.bashCompleter "file")
  )

pTxInOnly :: Parser TxIn
pTxInOnly =
    Opt.option
        (readerFromParsecParser parseTxIn)
        ( Opt.long "tx-in-only"
            <> Opt.metavar "TX-IN"
            <> Opt.help "TxId#TxIx"
        )

pTxInOref :: Parser GYTxOutRef
pTxInOref =
    Opt.option
        (txOutRefFromApi <$> readerFromParsecParser parseTxIn)
        ( Opt.long "tx-oref"
            <> Opt.metavar "TxId#TxIx"
            <> Opt.help "Required TxOutRef (reference to a Tx input)."
        )

pTxInRefOref :: Parser GYTxOutRef
pTxInRefOref =
    Opt.option
        (txOutRefFromApi <$> readerFromParsecParser parseTxIn)
        ( Opt.long "read-only-tx-in-reference"
            <> Opt.metavar "TxId#TxIx"
            <> Opt.help "Read only TxOutRef."
        )

pReward :: Parser GYValue
pReward = GY.valueFromLovelace <$> Opt.option Opt.auto
  ( Opt.long "lovelace-reward"
      <> Opt.metavar "INTEGER"
      <> Opt.help "Reward lovelace value."
  )

pSubmitTx :: Parser Bool
pSubmitTx =
  Opt.option Opt.auto
      ( Opt.long "submit-tx"
          <> Opt.metavar "BOOL"
          <> Opt.value False
          <> Opt.showDefault
          <> Opt.help "Whether to submit the Tx (true or false)."
      )

pMessage' :: Parser String
pMessage' =
  Opt.strOption
    ( Opt.long "message"
        <> Opt.metavar "MESSAGE"
        <> Opt.help "Asterizm message."
    )

----- :parsing TxInputInfo: -----

data TxInputInfo = TxInputInfo
  { txinOref   :: !GYTxOutRef
  , txinScript :: !ScriptInput
  } deriving stock Show

data ScriptInput = ScriptInput FilePath ScriptDataOrFile ScriptRefOref
                 | PubKeyInput
                 deriving stock Show

data ScriptRefOref = ScriptRefOref GYTxOutRef
                   | NotScriptRef
                   deriving stock Show

pTxInputInfo :: Parser TxInputInfo
pTxInputInfo =
  TxInputInfo
    <$> (txOutRefFromApi <$>
           Opt.option
             (readerFromParsecParser parseTxIn)
             ( Opt.long "tx-in"
               <> Opt.metavar "TxId#TxIx"
               <> Opt.help "Required TxOutRef (reference to a Tx input)."
             )
        )
    <*> pScriptInput

pScriptInput :: Parser ScriptInput
pScriptInput =
  ( ScriptInput <$>
      Opt.strOption
        ( Opt.long "script-file"
            <> Opt.metavar "FILEPATH"
            <> Opt.help "Path to script file."
            <> Opt.completer (Opt.bashCompleter "file")
        )
    <*> pScriptRedeemerOrFile "spending-reference-tx-in"
    <*> pScriptRefOref
  )
  <|> pure PubKeyInput

pScriptRefOref :: Parser ScriptRefOref
pScriptRefOref =
  ( Opt.option
      (ScriptRefOref . txOutRefFromApi <$> readerFromParsecParser parseTxIn)
      ( Opt.long "script-reference-oref"
        <> Opt.metavar "TxId#TxIx"
        <> Opt.help "TxOutRef to reference script."
      )
  )
  <|> pure NotScriptRef

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

----- :parsing Message: -----

pMessageString :: Parser BS.ByteString
pMessageString = BS.pack <$> Opt.strOption
  ( Opt.long "message"
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

data StageTx = RollupInit | RollupPark | RollupDataPark | RollupUpdate | RollupClear
             | TokenInit | TokenTransfer | TokenMinting | TokenBurning
             | VerifierTransfer | VerifierTx

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
  outFileFlag RollupInit       = "rollup-init-out-file"
  outFileFlag RollupPark       = "rollup-park-out-file"
  outFileFlag RollupDataPark   = "rollup-data-park-out-file"
  outFileFlag RollupUpdate     = "rollup-update-out-file"
  outFileFlag RollupClear      = "rollup-clear-out-file"
  outFileFlag TokenInit        = "token-init-out-file"
  outFileFlag TokenTransfer    = "token-transfer-out-file"
  outFileFlag TokenMinting     = "token-mint-out-file"
  outFileFlag TokenBurning     = "token-burn-out-file"
  outFileFlag VerifierTransfer = "verifier-transfer-out-file"
  outFileFlag VerifierTx       = "verifier-tx-out-file"

  outFileName RollupInit       = "rollup-init.tx"
  outFileName RollupPark       = "rollup-park.tx"
  outFileName RollupDataPark   = "rollup-data-park.tx"
  outFileName RollupUpdate     = "rollup-update.tx"
  outFileName RollupClear      = "rollup-clear.tx"
  outFileName TokenInit        = "token-init.tx"
  outFileName TokenTransfer    = "token-transfer.tx"
  outFileName TokenMinting     = "token-mint.tx"
  outFileName TokenBurning     = "token-burn.tx"
  outFileName VerifierTransfer = "verifier-transfer.tx"
  outFileName VerifierTx       = "verifier-tx.tx"

  outFileHelp RollupInit       = "Path (relative to 'assets/') for rollup initialization tx out-file."
  outFileHelp RollupPark       = "Path (relative to 'assets/') for 'rollup' script-parking tx out-file."
  outFileHelp RollupDataPark   = "Path (relative to 'assets/') for 'rollupData' script-parking tx out-file."
  outFileHelp RollupUpdate     = "Path (relative to 'assets/') for rollup update tx out-file."
  outFileHelp RollupClear      = "Path (relative to 'assets/') for rollup token-clearing tx out-file."
  outFileHelp TokenInit        = "Path (relative to 'assets/') for plonkupVerifierToken initialization tx out-file."
  outFileHelp TokenTransfer    = "Path (relative to 'assets/') for plonkupVerifierToken reward transfer tx out-file."
  outFileHelp TokenMinting     = "Path (relative to 'assets/') for plonkupVerifierToken minting tx out-file."
  outFileHelp TokenBurning     = "Path (relative to 'assets/') for plonkupVerifierToken burning tx out-file."
  outFileHelp VerifierTransfer = "Path (relative to 'assets/') for plonkupVerifierTx transfer tx out-file."
  outFileHelp VerifierTx       = "Path (relative to 'assets/') for plonkupVerifierTx verification tx out-file."
