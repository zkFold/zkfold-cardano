module ZkFold.Cardano.Options.ZkCLI where

import           Cardano.Api                                              (Doc, ExceptT (..))
import           Cardano.CLI.Parser                                       (commandWithMetavar)
import           Data.Maybe                                               (catMaybes)
import           GeniusYield.GYConfig                                     (GYCoreConfig)
import           Options.Applicative                                      (Parser, ParserInfo, ParserPrefs, asum,
                                                                           (<**>))
import qualified Options.Applicative                                      as Opt
import           Prelude

import qualified ZkFold.Cardano.Asterizm.Transaction.Minting              as AsterizmMinting
import           ZkFold.Cardano.Options.Common
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Burning  as TokenBurning
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Init     as TokenInit
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Minting  as TokenMinting
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Transfer as TokenTransfer
import qualified ZkFold.Cardano.Rollup.Transaction.Clear                  as RollupClear
import qualified ZkFold.Cardano.Rollup.Transaction.Init                   as RollupInit
import qualified ZkFold.Cardano.Rollup.Transaction.Update                 as RollupUpdate


data ClientCommand
    = TransactionAsterizmMinting AsterizmMinting.Transaction
    | TransactionTokenInit     TokenInit.Transaction
    | TransactionTokenTransfer TokenTransfer.Transaction
    | TransactionTokenMinting  TokenMinting.Transaction
    | TransactionTokenBurning  TokenBurning.Transaction
--    | TransactionVerifierInit     VerifierInit.Transaction
--    | TransactionVerifierTransfer VerifierTransfer.Transaction
--    | TransactionVerifierTx       VerifierTx.Transaction
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
            [ fmap TransactionAsterizmMinting   <$> pTransactionAsterizmMinting path mcfg
            , fmap TransactionTokenInit         <$> pTransactionTokenInit path mcfg
            , fmap TransactionTokenTransfer     <$> pTransactionTokenTransfer path mcfg
            , fmap TransactionTokenMinting      <$> pTransactionTokenMinting path mcfg
            , fmap TransactionTokenBurning      <$> pTransactionTokenBurning path mcfg
            , fmap TransactionRollupInit        <$> pTransactionRollupInit path mcfg
            , fmap TransactionRollupUpdate      <$> pTransactionRollupUpdate path mcfg
            , fmap TransactionRollupClear       <$> pTransactionRollupClear path mcfg
            -- , fmap TransactionVerifierInit      <$> pTransactionVerifierInit path mcfg
            -- , fmap TransactionVerifierTransfer  <$> pTransactionVerifierTransfer path mcfg
            -- , fmap TransactionVerifierTx        <$> pTransactionVerifierTx path mcfg
            ]

pTransactionAsterizmMinting :: FilePath -> Maybe GYCoreConfig -> Maybe (Parser AsterizmMinting.Transaction)
pTransactionAsterizmMinting path mcfg = do
    pure $ subParser "asterizm-mint" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        AsterizmMinting.Transaction path
            <$> pGYCoreConfig' mcfg
            <*> pSigningKeyAlt
            <*> pBenefOutAddress'
            <*> pMessage
            <*> pSubmitTx

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

pTransactionRollupInit :: FilePath -> Maybe GYCoreConfig -> Maybe (Parser RollupInit.Transaction)
pTransactionRollupInit path mcfg = do
    pure $ subParser "rollup-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        RollupInit.Transaction path
            <$> pGYCoreConfig' mcfg
            <*> pSigningKeyAlt
            <*> pChangeAddress'
            <*> pTxInOref
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

-- pTransactionVerifierInit :: FilePath -> Maybe GYCoreConfig -> Maybe (Parser VerifierInit.Transaction)
-- pTransactionVerifierInit path mcfg = do
--     pure $ subParser "plonkup-verifier-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
--   where
--     pCmd = do
--       VerifierInit.Transaction path
--             <$> pGYCoreConfig' mcfg

-- pTransactionVerifierTransfer :: FilePath -> Maybe GYCoreConfig -> Maybe (Parser VerifierTransfer.Transaction)
-- pTransactionVerifierTransfer path mcfg = do
--     pure $ subParser "plonkup-verifier-transfer" $ Opt.info pCmd $ Opt.progDescDoc Nothing
--   where
--     pCmd = do
--       VerifierTransfer.Transaction path
--             <$> pGYCoreConfig' mcfg
--             <*> pReward
--             <*> pSigningKeyAlt
--             <*> pChangeAddress'
--             <*> pOutFile' VerifierTransfer

-- pTransactionVerifierTx :: FilePath -> Maybe GYCoreConfig -> Maybe (Parser VerifierTx.Transaction)
-- pTransactionVerifierTx path mcfg = do
--     pure $ subParser "plonkup-verifier-tx" $ Opt.info pCmd $ Opt.progDescDoc Nothing
--   where
--     pCmd = do
--       VerifierTx.Transaction path
--             <$> pGYCoreConfig' mcfg
--             <*> pSigningKeyAlt
--             <*> pChangeAddress'
--             <*> many pTxInputInfo
--             <*> many pTxInRefOref
--             <*> many (pTxOutEraAware ShelleyBasedEraConway)
--             <*> pSubmitTx
--             <*> pOutFile' VerifierTx

data ClientCommandErrors

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand = \case
    TransactionAsterizmMinting   cmd -> ExceptT (Right <$> AsterizmMinting.mint                cmd)
    TransactionTokenInit         cmd -> ExceptT (Right <$> TokenInit.tokenInit                 cmd)
    TransactionTokenTransfer     cmd -> ExceptT (Right <$> TokenTransfer.tokenTransfer         cmd)
    TransactionTokenMinting      cmd -> ExceptT (Right <$> TokenMinting.tokenMinting           cmd)
    TransactionTokenBurning      cmd -> ExceptT (Right <$> TokenBurning.tokenBurning           cmd)
    TransactionRollupInit        cmd -> ExceptT (Right <$> RollupInit.rollupInit               cmd)
    TransactionRollupUpdate      cmd -> ExceptT (Right <$> RollupUpdate.rollupUpdate           cmd)
    TransactionRollupClear       cmd -> ExceptT (Right <$> RollupClear.rollupClear             cmd)
    -- TransactionVerifierInit      cmd -> ExceptT (Right <$> VerifierInit.verifierInit           cmd)
    -- TransactionVerifierTransfer  cmd -> ExceptT (Right <$> VerifierTransfer.verifierTransfer   cmd)
    -- TransactionVerifierTx        cmd -> ExceptT (Right <$> VerifierTx.verifierTx               cmd)

renderClientCommandError :: ClientCommandErrors -> Doc ann
renderClientCommandError = undefined

subParser :: String -> Opt.ParserInfo a -> Opt.Parser a
subParser cmdName pInfo =
  Opt.hsubparser $ commandWithMetavar cmdName pInfo
