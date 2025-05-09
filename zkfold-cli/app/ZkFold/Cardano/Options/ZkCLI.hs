module ZkFold.Cardano.Options.ZkCLI where

import           Cardano.Api                                              (Doc, ExceptT (..))
import           Cardano.CLI.EraBased.Common.Option                       (pWitnessSigningData)
import           Cardano.CLI.Parser                                       (commandWithMetavar)
import           Data.Maybe                                               (catMaybes)
import           GeniusYield.GYConfig                                     (GYCoreConfig)
import           Options.Applicative                                      (Parser, ParserInfo, ParserPrefs, asum,
                                                                           (<**>))
import qualified Options.Applicative                                      as Opt
import           Prelude

import qualified ZkFold.Cardano.Balancing.Transaction.Balancing           as Balancing
import qualified ZkFold.Cardano.Balancing.Transaction.Init                as BalancingInit
import qualified ZkFold.Cardano.Balancing.Transaction.Transfer            as BalancingTransfer
import           ZkFold.Cardano.Options.Common
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Burning  as TokenBurning
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Init     as TokenInit
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Minting  as TokenMinting
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Transfer as TokenTransfer
import qualified ZkFold.Cardano.Rollup.Transaction.Clear                  as RollupClear
import qualified ZkFold.Cardano.Rollup.Transaction.Init                   as RollupInit
import qualified ZkFold.Cardano.Rollup.Transaction.Update                 as RollupUpdate


data ClientCommand
    = TransactionTokenInit     TokenInit.Transaction
    | TransactionTokenTransfer TokenTransfer.Transaction
    | TransactionTokenMinting  TokenMinting.Transaction
    | TransactionTokenBurning  TokenBurning.Transaction
    | TransactionBalancingInit     BalancingInit.Transaction
    | TransactionBalancingTransfer BalancingTransfer.Transaction
    | TransactionBalancing         Balancing.Transaction
    | TransactionRollupInit   RollupInit.Transaction
    | TransactionRollupUpdate RollupUpdate.Transaction
    | TransactionRollupClear  RollupClear.Transaction

opts :: FilePath -> Maybe GYCoreConfig -> ParserInfo ClientCommand
opts path mcfg =
    Opt.info (pCmds path mcfg <**> Opt.helper) $
        mconcat
            [ Opt.fullDesc
            , Opt.header $
                mconcat
                  [ "zkfold-cli - Command-line utility to interact with cardano-node."
                  , " Provides specific commands to manage keys, addresses, build & submit transactions,"
                  , " certificates, etc."
                  ]
            ]

pref :: ParserPrefs
pref = Opt.prefs $ mconcat [] -- no help

pCmds :: FilePath -> Maybe GYCoreConfig -> Parser ClientCommand
pCmds path mcfg = do
    asum $
        catMaybes
            [ fmap TransactionTokenInit         <$> pTransactionTokenInit path mcfg
            , fmap TransactionTokenTransfer     <$> pTransactionTokenTransfer path mcfg
            , fmap TransactionTokenMinting      <$> pTransactionTokenMinting path mcfg
            , fmap TransactionTokenBurning      <$> pTransactionTokenBurning path mcfg
            , fmap TransactionBalancingInit     <$> pTransactionBalancingInit
            , fmap TransactionBalancingTransfer <$> pTransactionBalancingTransfer path
            , fmap TransactionBalancing         <$> pTransactionBalancing path
            , fmap TransactionRollupInit        <$> pTransactionRollupInit path mcfg
            , fmap TransactionRollupUpdate      <$> pTransactionRollupUpdate path mcfg
            , fmap TransactionRollupClear       <$> pTransactionRollupClear path mcfg
            ]

pTransactionTokenInit :: FilePath -> Maybe GYCoreConfig -> Maybe (Parser TokenInit.Transaction)
pTransactionTokenInit path mcfg = do
    pure $ subParser "token-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        TokenInit.Transaction path
            <$> pGYCoreConfig' mcfg
            <*> pFMTag
            <*> pSigningKeyAlt
            <*> pChangeAddress'
            <*> pParkOutAddress'
            <*> pOutFile

pTransactionTokenTransfer :: FilePath -> Maybe GYCoreConfig -> Maybe (Parser TokenTransfer.Transaction)
pTransactionTokenTransfer path mcfg = do
    pure $ subParser "token-transfer" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        TokenTransfer.Transaction path
            <$> pGYCoreConfig' mcfg
            <*> pFMTag
            <*> pPolicyIdAlt
            <*> pReward
            <*> pSigningKeyAlt
            <*> pChangeAddress'
            <*> pOutFile

pTransactionTokenMinting :: FilePath -> Maybe GYCoreConfig -> Maybe (Parser TokenMinting.Transaction)
pTransactionTokenMinting path mcfg = do
    pure $ subParser "token-mint" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        TokenMinting.Transaction path
            <$> pGYCoreConfig' mcfg
            <*> pSigningKeyAlt
            <*> pChangeAddress'
            <*> pBenefOutAddress'
            <*> pTxIdAlt
            <*> pOutFile

pTransactionTokenBurning :: FilePath -> Maybe GYCoreConfig -> Maybe (Parser TokenBurning.Transaction)
pTransactionTokenBurning path mcfg = do
    pure $ subParser "token-burn" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        TokenBurning.Transaction path
            <$> pGYCoreConfig' mcfg
            <*> pFMTag
            <*> pSigningKeyAlt
            <*> pChangeAddress'
            <*> pTokenAlt
            <*> pTxIdAlt
            <*> pOutFile

pTransactionBalancingInit :: Maybe (Parser BalancingInit.Transaction)
pTransactionBalancingInit = do
    pure $ subParser "balancing-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        BalancingInit.Transaction
            <$> pGYCoreConfig
            <*> pTxInOnly
            <*> pWitnessSigningData
            <*> pChangeAddress
            <*> pOutFile

pTransactionBalancingTransfer :: FilePath -> Maybe (Parser BalancingTransfer.Transaction)
pTransactionBalancingTransfer path = do
    pure $ subParser "balancing-transfer" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        BalancingTransfer.Transaction path
            <$> pGYCoreConfig
            <*> pTxInOnly
            <*> pWitnessSigningData
            <*> pChangeAddress
            <*> pOutFile

pTransactionBalancing :: FilePath -> Maybe (Parser Balancing.Transaction)
pTransactionBalancing path = do
    pure $ subParser "balancing-something" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        Balancing.Transaction path
            <$> pGYCoreConfig
            <*> pTxInOnly
            <*> pTxInOnly
            <*> pWitnessSigningData
            <*> pChangeAddress
            <*> pOutFile
            <*> pTxIdFile
            <*> pOutAddress

pTransactionRollupInit :: FilePath -> Maybe GYCoreConfig -> Maybe (Parser RollupInit.Transaction)
pTransactionRollupInit path mcfg = do
    pure $ subParser "rollup-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        RollupInit.Transaction path
            <$> pGYCoreConfig' mcfg
            <*> pSigningKeyAlt
            <*> pChangeAddress'
            <*> pTxOref
            <*> pFeeAddress'
            <*> pOutFile' RollupInit
            <*> pOutFile' RollupPark
            <*> pOutFile' RollupDataPark

pTransactionRollupUpdate :: FilePath -> Maybe GYCoreConfig -> Maybe (Parser RollupUpdate.Transaction)
pTransactionRollupUpdate path mcfg = do
    pure $ subParser "rollup-update" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        RollupUpdate.Transaction path
            <$> pGYCoreConfig' mcfg
            <*> pSigningKeyAlt
            <*> pChangeAddress'
            <*> pOutFile' RollupPark
            <*> pOutFile' RollupDataPark
            <*> pOutFile' RollupUpdate

pTransactionRollupClear :: FilePath -> Maybe GYCoreConfig -> Maybe (Parser RollupClear.Transaction)
pTransactionRollupClear path mcfg = do
    pure $ subParser "rollup-clear" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        RollupClear.Transaction path
            <$> pGYCoreConfig' mcfg
            <*> pSigningKeyAlt
            <*> pChangeAddress'
            <*> pOutFile' RollupDataPark
            <*> pOutFile' RollupClear

data ClientCommandErrors

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand = \case
    TransactionTokenInit         cmd -> ExceptT (Right <$> TokenInit.tokenInit                 cmd)
    TransactionTokenTransfer     cmd -> ExceptT (Right <$> TokenTransfer.tokenTransfer         cmd)
    TransactionTokenMinting      cmd -> ExceptT (Right <$> TokenMinting.tokenMinting           cmd)
    TransactionTokenBurning      cmd -> ExceptT (Right <$> TokenBurning.tokenBurning           cmd)
    TransactionBalancingInit     cmd -> ExceptT (Right <$> BalancingInit.balancingInit         cmd)
    TransactionBalancingTransfer cmd -> ExceptT (Right <$> BalancingTransfer.balancingTransfer cmd)
    TransactionBalancing         cmd -> ExceptT (Right <$> Balancing.balancingPlonkup          cmd)
    TransactionRollupInit        cmd -> ExceptT (Right <$> RollupInit.rollupInit               cmd)
    TransactionRollupUpdate      cmd -> ExceptT (Right <$> RollupUpdate.rollupUpdate           cmd)
    TransactionRollupClear       cmd -> ExceptT (Right <$> RollupClear.rollupClear             cmd)

renderClientCommandError :: ClientCommandErrors -> Doc ann
renderClientCommandError = undefined

subParser :: String -> Opt.ParserInfo a -> Opt.Parser a
subParser cmdName pInfo =
  Opt.hsubparser $ commandWithMetavar cmdName pInfo
