module ZkFold.Cardano.Options.AsterizmCLI where

import           Cardano.Api                                  (Doc, ExceptT (..))
import           Cardano.CLI.Parser                           (commandWithMetavar)
import           Options.Applicative                          (Parser, ParserInfo, ParserPrefs, asum, many, (<**>))
import qualified Options.Applicative                          as Opt
import           Prelude

import qualified ZkFold.Cardano.Asterizm.Transaction.Client   as AsterizmClient
import qualified ZkFold.Cardano.Asterizm.Transaction.Hash     as AsterizmHash
import qualified ZkFold.Cardano.Asterizm.Transaction.Relayer  as AsterizmRelayer
import qualified ZkFold.Cardano.Asterizm.Transaction.Retrieve as AsterizmRetrieve
import           ZkFold.Cardano.CLI.Parsers
import           ZkFold.Cardano.Options.Common                hiding (pVerificationKeyFile)


data ClientCommand
    = TransactionAsterizmClientSend AsterizmClient.SendTransaction
    | TransactionAsterizmClientReceive AsterizmClient.ReceiveTransaction
    | TransactionAsterizmHash AsterizmHash.Transaction
    | TransactionAsterizmRelayer AsterizmRelayer.Transaction
    | TransactionAsterizmRetrieve AsterizmRetrieve.Transaction

opts :: ParserInfo ClientCommand
opts =
    Opt.info (pCmds <**> Opt.helper) $
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

pCmds :: Parser ClientCommand
pCmds = do
    asum $
        [ pTransactionAsterizmClient
        , TransactionAsterizmHash      <$> pTransactionAsterizmHash
        , TransactionAsterizmRelayer   <$> pTransactionAsterizmRelayer
        , TransactionAsterizmRetrieve  <$> pTransactionAsterizmRetrieve
        ]

-- | Parser for client subcommands (send/receive)
pTransactionAsterizmClient :: Parser ClientCommand
pTransactionAsterizmClient = do
    subParser "client" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = asum
        [ TransactionAsterizmClientSend <$> pClientSend
        , TransactionAsterizmClientReceive <$> pClientReceive
        ]

    pClientSend = subParser "send" $ Opt.info pSendCmd $ Opt.progDescDoc Nothing
      where
        pSendCmd = do
            AsterizmClient.SendTransaction
                <$> pGYCoreConfigFile
                <*> pSigningKeyFile
                <*> pVerificationKeyFile "client"
                <*> pBenefOutAddress
                <*> pMessage

    pClientReceive = subParser "receive" $ Opt.info pReceiveCmd $ Opt.progDescDoc Nothing
      where
        pReceiveCmd = do
            AsterizmClient.ReceiveTransaction
                <$> pGYCoreConfigFile
                <*> pSigningKeyFile
                <*> pVerificationKeyFile "client"
                <*> many (pVerificationKeyFile "relayer")
                <*> pBenefOutAddress
                <*> pMessage

pTransactionAsterizmHash :: Parser AsterizmHash.Transaction
pTransactionAsterizmHash = do
    subParser "hash" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = AsterizmHash.Transaction <$> pMessage

pTransactionAsterizmRelayer :: Parser AsterizmRelayer.Transaction
pTransactionAsterizmRelayer = do
    subParser "relayer" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        AsterizmRelayer.Transaction
            <$> pGYCoreConfigFile
            <*> pSigningKeyFile
            <*> pVerificationKeyFile "relayer"
            <*> pBenefOutAddress
            <*> pMessageHash

pTransactionAsterizmRetrieve :: Parser AsterizmRetrieve.Transaction
pTransactionAsterizmRetrieve = do
    subParser "retrieve-messages" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        AsterizmRetrieve.Transaction
            <$> pGYCoreConfigFile
            <*> pVerificationKeyFile "client"
            <*> many (pVerificationKeyFile "relayer")
            <*> pMessageDirection

data ClientCommandErrors

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand = \case
    TransactionAsterizmClientSend    cmd -> ExceptT (Right <$> AsterizmClient.clientSend    cmd)
    TransactionAsterizmClientReceive cmd -> ExceptT (Right <$> AsterizmClient.clientReceive cmd)
    TransactionAsterizmHash          cmd -> ExceptT (Right <$> AsterizmHash.computeHash     cmd)
    TransactionAsterizmRelayer       cmd -> ExceptT (Right <$> AsterizmRelayer.relayerMint  cmd)
    TransactionAsterizmRetrieve      cmd -> ExceptT (Right <$> AsterizmRetrieve.retrieveMsgs cmd)

renderClientCommandError :: ClientCommandErrors -> Doc ann
renderClientCommandError = undefined

subParser :: String -> Opt.ParserInfo a -> Opt.Parser a
subParser cmdName pInfo =
  Opt.hsubparser $ commandWithMetavar cmdName pInfo
