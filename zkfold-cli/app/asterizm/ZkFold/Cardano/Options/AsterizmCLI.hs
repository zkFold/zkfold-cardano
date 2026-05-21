module ZkFold.Cardano.Options.AsterizmCLI where

import           Cardano.Api                                  (Doc, ExceptT (..))
import           Cardano.CLI.Parser                           (commandWithMetavar)
import           Options.Applicative                          (Parser, ParserInfo, ParserPrefs, asum, many, (<**>))
import qualified Options.Applicative                          as Opt
import           Prelude

import qualified ZkFold.Cardano.Asterizm.Transaction.Client   as AsterizmClient
import qualified ZkFold.Cardano.Asterizm.Transaction.Hash     as AsterizmHash
import qualified ZkFold.Cardano.Asterizm.Transaction.Policy   as AsterizmPolicy
import qualified ZkFold.Cardano.Asterizm.Transaction.Relayer  as AsterizmRelayer
import qualified ZkFold.Cardano.Asterizm.Transaction.Retrieve as AsterizmRetrieve
import qualified ZkFold.Cardano.Asterizm.Transaction.User     as AsterizmUser
import           ZkFold.Cardano.CLI.Parsers
import           ZkFold.Cardano.Options.Common                hiding (pVerificationKeyFile)


data ClientCommand
    = TransactionAsterizmClientSend AsterizmClient.SendTransaction
    | TransactionAsterizmClientReceive AsterizmClient.ReceiveTransaction
    | TransactionAsterizmHash AsterizmHash.Transaction
    | TransactionAsterizmPolicyClient AsterizmPolicy.ClientTransaction
    | TransactionAsterizmPolicyRelayer AsterizmPolicy.RelayerTransaction
    | TransactionAsterizmPolicyUser AsterizmPolicy.UserTransaction
    | TransactionAsterizmRelayer AsterizmRelayer.Transaction
    | TransactionAsterizmRetrieve AsterizmRetrieve.Transaction
    | TransactionAsterizmUserSend AsterizmUser.SendTransaction

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
        , pTransactionAsterizmPolicy
        , TransactionAsterizmRelayer   <$> pTransactionAsterizmRelayer
        , TransactionAsterizmRetrieve  <$> pTransactionAsterizmRetrieve
        , pTransactionAsterizmUser
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
                <*> many pTrustedAddress
                <*> pBenefOutAddress
                <*> pHashMode
                <*> pMessage

    pClientReceive = subParser "receive" $ Opt.info pReceiveCmd $ Opt.progDescDoc Nothing
      where
        pReceiveCmd = do
            AsterizmClient.ReceiveTransaction
                <$> pGYCoreConfigFile
                <*> pSigningKeyFile
                <*> pVerificationKeyFile "client"
                <*> many (pVerificationKeyFile "relayer")
                <*> many pTrustedAddress
                <*> pBenefOutAddress
                <*> pHashMode
                <*> pMessage

pTransactionAsterizmHash :: Parser AsterizmHash.Transaction
pTransactionAsterizmHash =
    subParser "hash" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = AsterizmHash.Transaction <$> pHashMode <*> pMessage

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

-- | Parser for policy subcommands (client/relayer)
pTransactionAsterizmPolicy :: Parser ClientCommand
pTransactionAsterizmPolicy = do
    subParser "policy" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = asum
        [ TransactionAsterizmPolicyClient <$> pPolicyClient
        , TransactionAsterizmPolicyRelayer <$> pPolicyRelayer
        , TransactionAsterizmPolicyUser <$> pPolicyUser
        ]

    pPolicyClient = subParser "client" $ Opt.info pClientCmd $ Opt.progDescDoc Nothing
      where
        pClientCmd = do
            AsterizmPolicy.ClientTransaction
                <$> pVerificationKeyFile "client"
                <*> many (pVerificationKeyFile "relayer")
                <*> many pTrustedAddress
                <*> pMessageDirection

    pPolicyRelayer = subParser "relayer" $ Opt.info pRelayerCmd $ Opt.progDescDoc Nothing
      where
        pRelayerCmd = do
            AsterizmPolicy.RelayerTransaction
                <$> pVerificationKeyFile "relayer"

    pPolicyUser = subParser "user" $ Opt.info pUserCmd $ Opt.progDescDoc Nothing
      where
        pUserCmd = pure AsterizmPolicy.UserTransaction

-- | Parser for user subcommands (send)
pTransactionAsterizmUser :: Parser ClientCommand
pTransactionAsterizmUser = do
    subParser "user" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = asum
        [ TransactionAsterizmUserSend <$> pUserSend
        ]

    pUserSend = subParser "send" $ Opt.info pSendCmd $ Opt.progDescDoc Nothing
      where
        pSendCmd = do
            AsterizmUser.SendTransaction
                <$> pGYCoreConfigFile
                <*> pSigningKeyFile
                <*> pBenefOutAddress
                <*> pHashMode
                <*> pMessage

pTransactionAsterizmRetrieve :: Parser AsterizmRetrieve.Transaction
pTransactionAsterizmRetrieve = do
    subParser "retrieve-messages" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        AsterizmRetrieve.Transaction
            <$> pGYCoreConfigFile
            <*> pVerificationKeyFile "client"
            <*> many (pVerificationKeyFile "relayer")
            <*> many pTrustedAddress
            <*> pMessageDirection

data ClientCommandErrors

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand = \case
    TransactionAsterizmClientSend    cmd -> ExceptT (Right <$> AsterizmClient.clientSend    cmd)
    TransactionAsterizmClientReceive cmd -> ExceptT (Right <$> AsterizmClient.clientReceive cmd)
    TransactionAsterizmHash          cmd -> ExceptT (Right <$> AsterizmHash.computeHash     cmd)
    TransactionAsterizmPolicyClient  cmd -> ExceptT (Right <$> AsterizmPolicy.printClientPolicy  cmd)
    TransactionAsterizmPolicyRelayer cmd -> ExceptT (Right <$> AsterizmPolicy.printRelayerPolicy cmd)
    TransactionAsterizmPolicyUser    cmd -> ExceptT (Right <$> AsterizmPolicy.printUserPolicy    cmd)
    TransactionAsterizmRelayer       cmd -> ExceptT (Right <$> AsterizmRelayer.relayerMint  cmd)
    TransactionAsterizmRetrieve      cmd -> ExceptT (Right <$> AsterizmRetrieve.retrieveMsgs cmd)
    TransactionAsterizmUserSend      cmd -> ExceptT (Right <$> AsterizmUser.userSend        cmd)

renderClientCommandError :: ClientCommandErrors -> Doc ann
renderClientCommandError = undefined

subParser :: String -> Opt.ParserInfo a -> Opt.Parser a
subParser cmdName pInfo =
  Opt.hsubparser $ commandWithMetavar cmdName pInfo
