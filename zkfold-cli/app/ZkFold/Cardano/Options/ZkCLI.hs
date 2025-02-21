module ZkFold.Cardano.Options.ZkCLI where

import           Cardano.Api                                              (Doc, ExceptT (..))
import           Cardano.CLI.EraBased.Options.Common                      (pWitnessSigningData)
import           Cardano.CLI.Parser                                       (subParser)
import           Data.Maybe                                               (catMaybes)
import           Options.Applicative                                      (Parser, ParserInfo, ParserPrefs, asum,
                                                                           (<**>))
import qualified Options.Applicative                                      as Opt
import           Prelude

import           ZkFold.Cardano.Options.CardanoCLI                        (pChangeAddress, pGYCoreConfig, pOutAddress,
                                                                           pOutFile, pTxInOnly, pTxIdFile)
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Init     as Init
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Transfer as Transfer
import qualified ZkFold.Cardano.PlonkupVerifierToken.Transaction.Minting  as Minting

data ClientCommand
    = TransactionInit Init.Transaction
    | TransactionTransfer Transfer.Transaction
    | TransactionMinting Minting.Transaction

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
        [ fmap TransactionInit <$> pTransactionInit path
        , fmap TransactionTransfer <$> pTransactionTransfer
        , fmap TransactionMinting <$>  pTransactionMinting path
        ]

pTransactionInit :: FilePath -> Maybe (Parser Init.Transaction)
pTransactionInit path = do
    pure $ subParser "token-init" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        Init.Transaction path
          <$> pGYCoreConfig
          <*> pTxInOnly
          <*> pWitnessSigningData
          <*> pChangeAddress
          <*> pOutAddress
          <*> pOutFile

pTransactionTransfer :: Maybe (Parser Transfer.Transaction)
pTransactionTransfer = do
    pure $ subParser "token-transfer" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        Transfer.Transaction
          <$> pGYCoreConfig
          <*> pTxInOnly
          <*> pWitnessSigningData
          <*> pChangeAddress
          <*> pOutFile

pTransactionMinting :: FilePath -> Maybe (Parser Minting.Transaction)
pTransactionMinting path = do
    pure $ subParser "token-transfer" $ Opt.info pCmd $ Opt.progDescDoc Nothing
  where
    pCmd = do
        Minting.Transaction path
          <$> pGYCoreConfig
          <*> pTxInOnly
          <*> pWitnessSigningData
          <*> pChangeAddress
          <*> pOutAddress
          <*> pTxIdFile
          <*> pOutFile

data ClientCommandErrors

{-
  case (name, command) of
    --
    ("balancing", "init")    -> balancingInit path
    ("balancing", "plonkup") -> balancingPlonkup path
    --
    ("token", "init")        -> tokenInit path
    ("token", "transfer")    -> tokenTransfer
    ("token", "minting")     -> tokenMinting path
    ("token", "burning")     -> tokenBurning path
    --
    ("tx", "init")           -> txInit path
    ("tx", "transfer")       -> txTransfer path args
    ("tx", "withdraw")       -> txWithdraw path
    --
    ("rollup", "clear")      -> rollupClear path
    ("rollup", "init")       -> rollupInit path args
    ("rollup", "update")     -> rollupUpdate path
    --
    _                        -> print @String "why?"

-}

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand = \case
  TransactionInit     cmd -> ExceptT (Right <$> Init.tokenInit         cmd)
  TransactionTransfer cmd -> ExceptT (Right <$> Transfer.tokenTransfer cmd)
  TransactionMinting  cmd -> ExceptT (Right <$> Minting.tokenMinting   cmd)

renderClientCommandError :: ClientCommandErrors -> Doc ann
renderClientCommandError = undefined
