{-# LANGUAGE InstanceSigs #-}

module ZkFold.Cardano.Options.Common where

import           Cardano.Api                        (parseAddressAny)
import           Cardano.CLI.EraBased.Common.Option (parseFilePath, parseTxIn, readerFromParsecParser)
import           Control.Exception                  (throwIO)
import qualified Data.ByteString.Char8              as BS
import           GeniusYield.GYConfig               as GY
import           GeniusYield.Types                  as GY
import           Options.Applicative                (Parser)
import qualified Options.Applicative                as Opt
import           Prelude

--------------------------- :Defaults: ---------------------------

-- | Name of data tokens mint Tx out-file.
dataOut :: String
dataOut = "dataTokens.tx"

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

pTxInOref :: Parser GYTxOutRef
pTxInOref =
    Opt.option
        (txOutRefFromApi <$> readerFromParsecParser parseTxIn)
        ( Opt.long "tx-oref"
            <> Opt.metavar "TxId#TxIx"
            <> Opt.help "Required TxOutRef (reference to a Tx input)."
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

----- :parsing OutFile: -----

data StageTx = RollupInit | RollupPark | RollupDataPark | RollupUpdate | RollupClear

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
  outFileFlag RollupInit     = "init-out-file"
  outFileFlag RollupPark     = "park-out-file"
  outFileFlag RollupDataPark = "data-park-out-file"
  outFileFlag RollupUpdate   = "update-out-file"
  outFileFlag RollupClear    = "clear-out-file"

  outFileName RollupInit     = "rollup-init.tx"
  outFileName RollupPark     = "rollup-park.tx"
  outFileName RollupDataPark = "rollup-data-park.tx"
  outFileName RollupUpdate   = "rollup-update.tx"
  outFileName RollupClear    = "rollup-clear.tx"

  outFileHelp RollupInit     = "Path (relative to 'assets/') for rollup initialization tx out-file."
  outFileHelp RollupPark     = "Path (relative to 'assets/') for 'rollup' script-parking tx out-file."
  outFileHelp RollupDataPark = "Path (relative to 'assets/') for 'rollupData' script-parking tx out-file."
  outFileHelp RollupUpdate   = "Path (relative to 'assets/') for rollup update tx out-file."
  outFileHelp RollupClear    = "Path (relative to 'assets/') for rollup token-clearing tx out-file."
