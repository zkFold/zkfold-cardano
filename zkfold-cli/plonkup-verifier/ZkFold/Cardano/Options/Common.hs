{-# LANGUAGE InstanceSigs #-}

module ZkFold.Cardano.Options.Common where

import           Cardano.Api                        (parseAddressAny)
import           Cardano.CLI.EraBased.Common.Option (pScriptRedeemerOrFile, parseFilePath, parseTxIn,
                                                     readerFromParsecParser)
import           Cardano.CLI.Type.Common            (ScriptDataOrFile)
import           Control.Exception                  (throwIO)
import qualified Data.ByteString.Char8              as BS
import           GeniusYield.GYConfig               as GY
import           GeniusYield.Types                  as GY
import           Options.Applicative                (Parser, (<|>))
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

--------------------------- :Parsers: ----------------------------

pChangeAddress :: Parser GYAddress
pChangeAddress =
  Opt.option (readerFromParsecParser $ fmap GY.addressFromApi parseAddressAny) $
      mconcat
          [ Opt.long "change-address"
          , Opt.metavar "ADDRESS"
          , Opt.help "Address where ADA in excess of the tx fee will go to."
          ]

pGYCoreConfig :: Maybe GYCoreConfig -> Parser CoreConfigAlt
pGYCoreConfig mcfg = Opt.option (CoreConfigUseFile <$> Opt.maybeReader Just)
  ( Opt.long "core-config-file"
      <> Opt.value (CoreConfigUseGY mcfg)
      <> Opt.metavar "FILEPATH"
      <> Opt.help "Path to 'core config'.  This overrides the CORE_CONFIG_PATH environment variable.  The argument is optional if CORE_CONFIG_PATH is defined and mandatory otherwise."
      <> Opt.completer (Opt.bashCompleter "file")
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

----- :parsing OutFile: -----

data StageTx = VerifierTransfer | VerifierTx

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
  outFileFlag VerifierTransfer = "transfer-out-file"
  outFileFlag VerifierTx       = "tx-out-file"

  outFileName VerifierTransfer = "verifier-transfer.tx"
  outFileName VerifierTx       = "verifier-tx.tx"

  outFileHelp VerifierTransfer = "Path (relative to 'assets/') for plonkupVerifierTx transfer tx out-file."
  outFileHelp VerifierTx       = "Path (relative to 'assets/') for plonkupVerifierTx verification tx out-file."
