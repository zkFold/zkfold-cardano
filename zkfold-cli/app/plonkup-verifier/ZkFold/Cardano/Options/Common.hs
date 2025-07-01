module ZkFold.Cardano.Options.Common where

import           Cardano.CLI.EraBased.Common.Option (pScriptRedeemerOrFile, parseTxIn, readerFromParsecParser)
import           Cardano.CLI.Type.Common            (ScriptDataOrFile)
import           GeniusYield.Types                  as GY
import           Options.Applicative                (Parser, (<|>))
import qualified Options.Applicative                as Opt
import           Prelude

----- :simple parsers: -----

pTxInRefOref :: Parser GYTxOutRef
pTxInRefOref =
    Opt.option
        (txOutRefFromApi <$> readerFromParsecParser parseTxIn)
        ( Opt.long "read-only-tx-in-reference"
            <> Opt.metavar "TxId#TxIx"
            <> Opt.help "Read only TxOutRef."
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
