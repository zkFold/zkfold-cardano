module Main where

import           Balancing.Balancing                       (balancingInit, balancingPlonkup)
import           Control.Monad                             (when)
import           PlonkupVerifierToken.PlonkupVerifierToken (tokenInit, tokenMinting, tokenTransfer)
import           PlonkupVerifierTx.PlonkupVerifierTx       (txInit, txTransfer, txWithdraw)
import           Prelude                                   (Eq (..), IO, String, head, null, print, tail, ($))
import           Rollup.Rollup                             (rollupClear, rollupInit, rollupUpdate)
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
  (name : argsHelp) <- getArgs

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
    ("token", "minting")     -> tokenMinting path
    ("token", "transfer")    -> tokenTransfer path args
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
