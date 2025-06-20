module ZkFold.Cardano.Options.RollupCLI where

import           Cardano.Api                                  (Doc, ExceptT (..))
import           Cardano.CLI.Parser                           (commandWithMetavar)
import           GeniusYield.GYConfig                         (GYCoreConfig)
import           Options.Applicative                          (Parser, ParserInfo, ParserPrefs, asum, (<**>))
import qualified Options.Applicative                          as Opt
import           Prelude

import           ZkFold.Cardano.Options.Common
import qualified ZkFold.Cardano.Rollup.Transaction.Clear      as RollupClear
import qualified ZkFold.Cardano.Rollup.Transaction.Init       as RollupInit
import qualified ZkFold.Cardano.Rollup.Transaction.Update     as RollupUpdate


data ClientCommand
    = TransactionRollupInit   RollupInit.Transaction
    | TransactionRollupUpdate RollupUpdate.Transaction
    | TransactionRollupClear  RollupClear.Transaction

opts :: FilePath -> Maybe GYCoreConfig -> ParserInfo ClientCommand
opts path mcfg =
    Opt.info (pCmds path mcfg <**> Opt.helper) $
        mconcat
            [ Opt.fullDesc
            , Opt.header $
                mconcat
                  [ "zkfold-cli:rollup - Command-line utility to interact with Cardano."
                  , " Provides specific commands to manage rollups."
                  ]
            ]

pref :: ParserPrefs
pref = Opt.prefs $ mconcat [] -- no help

pCmds :: FilePath -> Maybe GYCoreConfig -> Parser ClientCommand
pCmds path mcfg = do
    asum $
        [ TransactionRollupInit   <$> pTransactionRollupInit path mcfg
        , TransactionRollupUpdate <$> pTransactionRollupUpdate path mcfg
        , TransactionRollupClear  <$> pTransactionRollupClear path mcfg
        ]

pTransactionRollupInit :: FilePath -> Maybe GYCoreConfig -> Parser RollupInit.Transaction
pTransactionRollupInit path mcfg = do
    subParser "init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        RollupInit.Transaction path
            <$> pGYCoreConfig mcfg
            <*> pSigningKeyAlt
            <*> pChangeAddress
            <*> pTxInOref
            <*> pFeeAddress
            <*> pOutFile RollupInit
            <*> pOutFile RollupPark
            <*> pOutFile RollupDataPark

pTransactionRollupUpdate :: FilePath -> Maybe GYCoreConfig -> Parser RollupUpdate.Transaction
pTransactionRollupUpdate path mcfg = do
    subParser "update" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        RollupUpdate.Transaction path
            <$> pGYCoreConfig mcfg
            <*> pSigningKeyAlt
            <*> pChangeAddress
            <*> pOutFile RollupPark
            <*> pOutFile RollupDataPark
            <*> pOutFile RollupUpdate

pTransactionRollupClear :: FilePath -> Maybe GYCoreConfig -> Parser RollupClear.Transaction
pTransactionRollupClear path mcfg = do
    subParser "clear" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        RollupClear.Transaction path
            <$> pGYCoreConfig mcfg
            <*> pSigningKeyAlt
            <*> pChangeAddress
            <*> pOutFile RollupDataPark
            <*> pOutFile RollupClear

data ClientCommandErrors

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand = \case
    TransactionRollupInit   cmd -> ExceptT (Right <$> RollupInit.rollupInit     cmd)
    TransactionRollupUpdate cmd -> ExceptT (Right <$> RollupUpdate.rollupUpdate cmd)
    TransactionRollupClear  cmd -> ExceptT (Right <$> RollupClear.rollupClear   cmd)

renderClientCommandError :: ClientCommandErrors -> Doc ann
renderClientCommandError = undefined

subParser :: String -> Opt.ParserInfo a -> Opt.Parser a
subParser cmdName pInfo =
  Opt.hsubparser $ commandWithMetavar cmdName pInfo
