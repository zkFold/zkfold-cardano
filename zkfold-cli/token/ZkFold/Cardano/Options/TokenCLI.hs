module ZkFold.Cardano.Options.TokenCLI where

import           Cardano.Api                                  (Doc, ExceptT (..))
import           Cardano.CLI.Parser                           (commandWithMetavar)
import           GeniusYield.GYConfig                         (GYCoreConfig)
import           Options.Applicative                          (Parser, ParserInfo, ParserPrefs, asum, (<**>))
import qualified Options.Applicative                          as Opt
import           Prelude

import           ZkFold.Cardano.Options.Common
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Init     as TokenInit
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Transfer as TokenTransfer
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Minting  as TokenMinting
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Burning  as TokenBurning


data ClientCommand
    = TransactionTokenInit     TokenInit.Transaction
    | TransactionTokenTransfer TokenTransfer.Transaction
    | TransactionTokenMinting  TokenMinting.Transaction
    | TransactionTokenBurning  TokenBurning.Transaction

opts :: FilePath -> Maybe GYCoreConfig -> ParserInfo ClientCommand
opts path mcfg =
    Opt.info (pCmds path mcfg <**> Opt.helper) $
        mconcat
            [ Opt.fullDesc
            , Opt.header $
                mconcat
                  [ "zkfold-cli:token - Command-line utility to interact with Cardano."
                  , " Provides specific commands to manage keys, addresses, build & submit transactions,"
                  , " etc. in the context of interacting with 'plonkupVerifierToken'."
                  ]
            ]

pref :: ParserPrefs
pref = Opt.prefs $ mconcat [] -- no help

pCmds :: FilePath -> Maybe GYCoreConfig -> Parser ClientCommand
pCmds path mcfg = do
    asum $
        [ TransactionTokenInit     <$> pTransactionTokenInit path mcfg
        , TransactionTokenTransfer <$> pTransactionTokenTransfer path mcfg
        , TransactionTokenMinting  <$> pTransactionTokenMinting path mcfg
        , TransactionTokenBurning  <$> pTransactionTokenBurning path mcfg
        ]

pTransactionTokenInit :: FilePath -> Maybe GYCoreConfig -> Parser TokenInit.Transaction
pTransactionTokenInit path mcfg = do
    subParser "init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        TokenInit.Transaction path
            <$> pGYCoreConfig mcfg
            <*> pFMTag
            <*> pSigningKeyAlt
            <*> pChangeAddress
            <*> pParkOutAddress
            <*> pOutFile TokenInit

pTransactionTokenTransfer :: FilePath -> Maybe GYCoreConfig -> Parser TokenTransfer.Transaction
pTransactionTokenTransfer path mcfg = do
    subParser "transfer" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        TokenTransfer.Transaction path
            <$> pGYCoreConfig mcfg
            <*> pFMTag
            <*> pPolicyIdAlt
            <*> pReward
            <*> pSigningKeyAlt
            <*> pChangeAddress
            <*> pOutFile TokenTransfer

pTransactionTokenMinting :: FilePath -> Maybe GYCoreConfig -> Parser TokenMinting.Transaction
pTransactionTokenMinting path mcfg = do
    subParser "mint" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        TokenMinting.Transaction path
            <$> pGYCoreConfig mcfg
            <*> pSigningKeyAlt
            <*> pChangeAddress
            <*> pBenefOutAddress
            <*> pTxIdAlt
            <*> pOutFile TokenMinting

pTransactionTokenBurning :: FilePath -> Maybe GYCoreConfig -> Parser TokenBurning.Transaction
pTransactionTokenBurning path mcfg = do
    subParser "burn" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        TokenBurning.Transaction path
            <$> pGYCoreConfig mcfg
            <*> pFMTag
            <*> pSigningKeyAlt
            <*> pChangeAddress
            <*> pTokenAlt
            <*> pTxIdAlt
            <*> pOutFile TokenBurning

data ClientCommandErrors

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand = \case
    TransactionTokenInit     cmd -> ExceptT (Right <$> TokenInit.tokenInit         cmd)
    TransactionTokenTransfer cmd -> ExceptT (Right <$> TokenTransfer.tokenTransfer cmd)
    TransactionTokenMinting  cmd -> ExceptT (Right <$> TokenMinting.tokenMinting   cmd)
    TransactionTokenBurning  cmd -> ExceptT (Right <$> TokenBurning.tokenBurning   cmd)

renderClientCommandError :: ClientCommandErrors -> Doc ann
renderClientCommandError = undefined

subParser :: String -> Opt.ParserInfo a -> Opt.Parser a
subParser cmdName pInfo =
  Opt.hsubparser $ commandWithMetavar cmdName pInfo
