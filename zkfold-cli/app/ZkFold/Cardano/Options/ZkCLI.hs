module ZkFold.Cardano.Options.ZkCLI where

import           Cardano.Api                                              (Doc, ExceptT (..))
import           Cardano.CLI.EraBased.Options.Common                      (pWitnessSigningData)
import           Cardano.CLI.Parser                                       (subParser)
import           Data.Maybe                                               (catMaybes)
import           Options.Applicative                                      (Parser, ParserInfo, ParserPrefs, asum,
                                                                           (<**>))
import qualified Options.Applicative                                      as Opt
import           Prelude

import qualified ZkFold.Cardano.Balancing.Transaction.Balancing           as Balancing
import qualified ZkFold.Cardano.Balancing.Transaction.Init                as BalancingInit
import qualified ZkFold.Cardano.Balancing.Transaction.Transfer            as BalancingTransfer
import           ZkFold.Cardano.Options.CardanoCLI                        (pChangeAddress, pGYCoreConfig, pOutAddress,
                                                                           pOutFile, pTxIdFile, pTxInOnly)
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Burning  as TokenBurning
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Init     as TokenInit
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Minting  as TokenMinting
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Transfer as TokenTransfer
import qualified ZkFold.Cardano.Rollup.Transaction.Init                   as RollupInit
import qualified ZkFold.Cardano.Rollup.Transaction.Next                   as RollupNext
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
    | TransactionRollupNext   RollupNext.Transaction

opts :: FilePath -> ParserInfo ClientCommand
opts path =
    Opt.info (pCmds path <**> Opt.helper) $
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

pCmds :: FilePath -> Parser ClientCommand
pCmds path = do
    asum $
        catMaybes
            [ fmap TransactionTokenInit         <$> pTransactionTokenInit path
            , fmap TransactionTokenTransfer     <$> pTransactionTokenTransfer
            , fmap TransactionTokenMinting      <$> pTransactionTokenMinting path
            , fmap TransactionTokenBurning      <$> pTransactionTokenBurning path
            , fmap TransactionBalancingInit     <$> pTransactionBalancingInit
            , fmap TransactionBalancingTransfer <$> pTransactionBalancingTransfer path
            , fmap TransactionBalancing         <$> pTransactionBalancing path
            , fmap TransactionRollupInit        <$> pTransactionRollupInit path
            , fmap TransactionRollupUpdate      <$> pTransactionRollupUpdate path
            , fmap TransactionRollupNext        <$> pTransactionRollupNext path
            ]

pTransactionTokenInit :: FilePath -> Maybe (Parser TokenInit.Transaction)
pTransactionTokenInit path = do
    pure $ subParser "token-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        TokenInit.Transaction path
            <$> pGYCoreConfig
            <*> pTxInOnly
            <*> pWitnessSigningData
            <*> pChangeAddress
            <*> pOutAddress
            <*> pOutFile

pTransactionTokenTransfer :: Maybe (Parser TokenTransfer.Transaction)
pTransactionTokenTransfer = do
    pure $ subParser "token-transfer" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        TokenTransfer.Transaction
            <$> pGYCoreConfig
            <*> pTxInOnly
            <*> pWitnessSigningData
            <*> pChangeAddress
            <*> pOutFile

pTransactionTokenMinting :: FilePath -> Maybe (Parser TokenMinting.Transaction)
pTransactionTokenMinting path = do
    pure $ subParser "token-minting" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        TokenMinting.Transaction path
            <$> pGYCoreConfig
            <*> pTxInOnly
            <*> pWitnessSigningData
            <*> pChangeAddress
            <*> pOutAddress
            <*> pTxIdFile
            <*> pOutFile

pTransactionTokenBurning :: FilePath -> Maybe (Parser TokenBurning.Transaction)
pTransactionTokenBurning path = do
    pure $ subParser "token-minting" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        TokenBurning.Transaction path
            <$> pGYCoreConfig
            <*> pTxInOnly
            <*> pTxInOnly
            <*> pTxInOnly
            <*> pWitnessSigningData
            <*> pChangeAddress
            <*> pOutAddress
            <*> pTxIdFile
            <*> pTxIdFile

pTransactionBalancingInit :: Maybe (Parser BalancingInit.Transaction)
pTransactionBalancingInit = do
    pure $ subParser "token-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
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
    pure $ subParser "token-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
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
    pure $ subParser "token-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
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

pTransactionRollupInit :: FilePath -> Maybe (Parser RollupInit.Transaction)
pTransactionRollupInit path = do
    pure $ subParser "token-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        RollupInit.Transaction path
            <$> pGYCoreConfig
            <*> pTxInOnly
            <*> pTxInOnly
            <*> pWitnessSigningData
            <*> pChangeAddress
            <*> pOutFile
            <*> pOutAddress
            <*> undefined

pTransactionRollupUpdate :: FilePath -> Maybe (Parser RollupUpdate.Transaction)
pTransactionRollupUpdate path = do
    pure $ subParser "token-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        RollupUpdate.Transaction path
            <$> pGYCoreConfig
            <*> pTxInOnly
            <*> pWitnessSigningData
            <*> pChangeAddress
            <*> pOutFile
            <*> undefined

pTransactionRollupNext :: FilePath -> Maybe (Parser RollupNext.Transaction)
pTransactionRollupNext path = do
    pure $ subParser "token-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        RollupNext.Transaction path
            <$> pGYCoreConfig
            <*> pTxInOnly
            <*> pTxInOnly
            <*> pTxInOnly
            <*> pTxIdFile
            <*> pWitnessSigningData
            <*> pChangeAddress
            <*> pOutFile
            <*> undefined
            <*> pOutAddress

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
    TransactionRollupNext        cmd -> ExceptT (Right <$> RollupNext.rollupNext               cmd)

renderClientCommandError :: ClientCommandErrors -> Doc ann
renderClientCommandError = undefined
