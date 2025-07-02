module ZkFold.Cardano.Options.AsterizmCLI where

import           Cardano.Api                                  (Doc, ExceptT (..))
import           Cardano.CLI.Parser                           (commandWithMetavar)
import           GeniusYield.GYConfig                         (GYCoreConfig)
import           Options.Applicative                          (Parser, ParserInfo, ParserPrefs, asum, many, (<**>))
import qualified Options.Applicative                          as Opt
import           Prelude

import qualified ZkFold.Cardano.Asterizm.Transaction.Client   as AsterizmClient
import qualified ZkFold.Cardano.Asterizm.Transaction.Init     as AsterizmInit
import qualified ZkFold.Cardano.Asterizm.Transaction.Message  as AsterizmMessage
import qualified ZkFold.Cardano.Asterizm.Transaction.Relayer  as AsterizmRelayer
import qualified ZkFold.Cardano.Asterizm.Transaction.Retrieve as AsterizmRetrieve
import           ZkFold.Cardano.CLI.Parsers
import           ZkFold.Cardano.Options.Common


data ClientCommand
    = TransactionAsterizmInit    AsterizmInit.Transaction
    | TransactionAsterizmClient  AsterizmClient.Transaction
    | TransactionAsterizmMessage AsterizmMessage.Transaction
    | TransactionAsterizmRelayer AsterizmRelayer.Transaction
    | TransactionAsterizmRetrieve AsterizmRetrieve.Transaction

opts :: FilePath -> Maybe GYCoreConfig -> ParserInfo ClientCommand
opts path mcfg =
    Opt.info (pCmds path mcfg <**> Opt.helper) $
        mconcat
            [ Opt.fullDesc
            , Opt.header $
                mconcat
                  [ "zkfold-cli:asterizm - Command-line utility to interact with Cardano."
                  , " Provides specific commands to manage the 'Asterizm' protocol."
                  ]
            ]

pref :: ParserPrefs
pref = Opt.prefs $ mconcat [] -- no help

pCmds :: FilePath -> Maybe GYCoreConfig -> Parser ClientCommand
pCmds path mcfg = do
    asum $
        [ TransactionAsterizmInit     <$> pTransactionAsterizmInit path mcfg
        , TransactionAsterizmClient   <$> pTransactionAsterizmClient path mcfg
        , TransactionAsterizmMessage  <$> pTransactionAsterizmMessage path
        , TransactionAsterizmRelayer  <$> pTransactionAsterizmRelayer path mcfg
        , TransactionAsterizmRetrieve <$> pTransactionAsterizmRetrieve path mcfg
        ]

pTransactionAsterizmInit :: FilePath -> Maybe GYCoreConfig -> Parser AsterizmInit.Transaction
pTransactionAsterizmInit path mcfg = do
    subParser "init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        AsterizmInit.Transaction path
            <$> pGYCoreConfig mcfg
            <*> pSigningKeyAlt
            <*> pTxInOref
            <*> pRegistryAddress
            <*> pPubKeyHashAlt Client
            <*> many (pPubKeyHashAlt Relayer)
            <*> pOutFile AsterizmInit

pTransactionAsterizmClient :: FilePath -> Maybe GYCoreConfig -> Parser AsterizmClient.Transaction
pTransactionAsterizmClient path mcfg = do
    subParser "client" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        AsterizmClient.Transaction path
            <$> pGYCoreConfig mcfg
            <*> pSigningKeyAlt
            <*> pBenefOutAddress
            <*> pMessageFile
            <*> pOutFile AsterizmClient

pTransactionAsterizmMessage :: FilePath -> Parser AsterizmMessage.Transaction
pTransactionAsterizmMessage path = do
    subParser "message" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        AsterizmMessage.Transaction path
            <$> pMessage
            <*> pMessageFile
            <*> pMessageHashFile

pTransactionAsterizmRelayer :: FilePath -> Maybe GYCoreConfig -> Parser AsterizmRelayer.Transaction
pTransactionAsterizmRelayer path mcfg = do
    subParser "relayer" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        AsterizmRelayer.Transaction path
            <$> pGYCoreConfig mcfg
            <*> pSigningKeyAlt
            <*> pBenefOutAddress
            <*> pMessageHashFile
            <*> pOutFile AsterizmRelayer

pTransactionAsterizmRetrieve :: FilePath -> Maybe GYCoreConfig -> Parser AsterizmRetrieve.Transaction
pTransactionAsterizmRetrieve path mcfg = do
    subParser "retrieve-messages" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        AsterizmRetrieve.Transaction path
            <$> pGYCoreConfig mcfg

data ClientCommandErrors

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand = \case
    TransactionAsterizmInit     cmd -> ExceptT (Right <$> AsterizmInit.asterizmInit     cmd)
    TransactionAsterizmClient   cmd -> ExceptT (Right <$> AsterizmClient.clientMint     cmd)
    TransactionAsterizmMessage  cmd -> ExceptT (Right <$> AsterizmMessage.clientMessage cmd)
    TransactionAsterizmRelayer  cmd -> ExceptT (Right <$> AsterizmRelayer.relayerMint   cmd)
    TransactionAsterizmRetrieve cmd -> ExceptT (Right <$> AsterizmRetrieve.retrieveMsgs cmd)

renderClientCommandError :: ClientCommandErrors -> Doc ann
renderClientCommandError = undefined

subParser :: String -> Opt.ParserInfo a -> Opt.Parser a
subParser cmdName pInfo =
  Opt.hsubparser $ commandWithMetavar cmdName pInfo
