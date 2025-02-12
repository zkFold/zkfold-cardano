module Main where

import           ZkFold.Cardano.Balancing.Balancing                       (balancingInit, balancingPlonkup)
import           Control.Monad                             (when)
import           GeniusYield.GYConfig                      (coreConfigIO)
import           ZkFold.Cardano.PlonkupVerifierToken.Transaction.Burning  (tokenBurning)
import           ZkFold.Cardano.PlonkupVerifierToken.Transaction.Init     (tokenInit)
import           ZkFold.Cardano.PlonkupVerifierToken.Transaction.Minting  (tokenMinting)
import           ZkFold.Cardano.PlonkupVerifierToken.Transaction.Transfer (tokenTransfer)
import           ZkFold.Cardano.PlonkupVerifierTx.PlonkupVerifierTx       (txInit, txTransfer, txWithdraw)
import           Prelude                                   (Eq (..), IO, String, head, null, print, tail, ($))
import           ZkFold.Cardano.Rollup.Rollup                             (rollupClear, rollupInit, rollupUpdate)
import           System.Directory                          (getCurrentDirectory)
import           System.Environment                        (getArgs)
import           System.FilePath                           (takeFileName)

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
          "e2e-test" -> ".."
          _          -> "."
  --
  (coreCfgPath : name : argsHelp) <- getArgs
  --
  coreCfg <- coreConfigIO coreCfgPath

  when (name == "help") $ print @String "help"

  when (null argsHelp) $ print @String "command?"

  let command = head argsHelp
      args    = tail argsHelp

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
