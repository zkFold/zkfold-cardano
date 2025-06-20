module ZkFold.Cardano.Options.VerifierCLI where

import           Cardano.Api                                  (Doc, ExceptT (..), ShelleyBasedEra (..))
import           Cardano.CLI.Parser                           (commandWithMetavar)
import           GeniusYield.GYConfig                         (GYCoreConfig)
import           Options.Applicative                          (Parser, ParserInfo, ParserPrefs, asum, many, (<**>))
import qualified Options.Applicative                          as Opt
import           Prelude

import           ZkFold.Cardano.Options.CardanoCLI
import           ZkFold.Cardano.Options.Common
import qualified ZkFold.Cardano.PlonkupVerifierTx.Transaction.Init     as VerifierInit
import qualified ZkFold.Cardano.PlonkupVerifierTx.Transaction.Transfer as VerifierTransfer
import qualified ZkFold.Cardano.PlonkupVerifierTx.Transaction.Tx       as VerifierTx


data ClientCommand
    = TransactionVerifierInit     VerifierInit.Transaction
    | TransactionVerifierTransfer VerifierTransfer.Transaction
    | TransactionVerifierTx       VerifierTx.Transaction

opts :: FilePath -> Maybe GYCoreConfig -> ParserInfo ClientCommand
opts path mcfg =
    Opt.info (pCmds path mcfg <**> Opt.helper) $
        mconcat
            [ Opt.fullDesc
            , Opt.header $
                mconcat
                  [ "zkfold-cli:plonkup-verifier - Command-line utility to interact with Cardano."
                  , " Provides specific commands to manage keys, addresses, build & submit transactions,"
                  , " etc. in the context of interacting with 'plonkupVerifierTx'."
                  ]
            ]

pref :: ParserPrefs
pref = Opt.prefs $ mconcat [] -- no help

pCmds :: FilePath -> Maybe GYCoreConfig -> Parser ClientCommand
pCmds path mcfg = do
    asum $
        [ TransactionVerifierInit     <$> pTransactionVerifierInit path mcfg
        , TransactionVerifierTransfer <$> pTransactionVerifierTransfer path mcfg
        , TransactionVerifierTx       <$> pTransactionVerifierTx path mcfg
        ]

pTransactionVerifierInit :: FilePath -> Maybe GYCoreConfig -> Parser VerifierInit.Transaction
pTransactionVerifierInit path mcfg = do
    subParser "init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
      VerifierInit.Transaction path
            <$> pGYCoreConfig mcfg

pTransactionVerifierTransfer :: FilePath -> Maybe GYCoreConfig -> Parser VerifierTransfer.Transaction
pTransactionVerifierTransfer path mcfg = do
    subParser "transfer" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
      VerifierTransfer.Transaction path
            <$> pGYCoreConfig mcfg
            <*> pReward
            <*> pSigningKeyAlt
            <*> pChangeAddress
            <*> pOutFile VerifierTransfer

pTransactionVerifierTx :: FilePath -> Maybe GYCoreConfig -> Parser VerifierTx.Transaction
pTransactionVerifierTx path mcfg = do
    subParser "tx" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
      VerifierTx.Transaction path
            <$> pGYCoreConfig mcfg
            <*> pSigningKeyAlt
            <*> pChangeAddress
            <*> many pTxInputInfo
            <*> many pTxInRefOref
            <*> many (pTxOutEraAware ShelleyBasedEraConway)
            <*> pSubmitTx
            <*> pOutFile VerifierTx

data ClientCommandErrors

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand = \case
    TransactionVerifierInit      cmd -> ExceptT (Right <$> VerifierInit.verifierInit         cmd)
    TransactionVerifierTransfer  cmd -> ExceptT (Right <$> VerifierTransfer.verifierTransfer cmd)
    TransactionVerifierTx        cmd -> ExceptT (Right <$> VerifierTx.verifierTx             cmd)

renderClientCommandError :: ClientCommandErrors -> Doc ann
renderClientCommandError = undefined

subParser :: String -> Opt.ParserInfo a -> Opt.Parser a
subParser cmdName pInfo =
  Opt.hsubparser $ commandWithMetavar cmdName pInfo
