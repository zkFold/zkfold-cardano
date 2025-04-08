module Main (main) where

import System.FilePath ((</>))
import ZkFold.Cardano.UPLC.Wallet.Compile (writeSmartWalletBP)
import Prelude

-- To be executed from root of zkfold-cardano repository.
main :: IO ()
main =
  let getPath fn = "zkfold-cardano-scripts-common" </> "data" </> "compiled-scripts" </> fn <> ".blueprint"
   in do
        writeSmartWalletBP $ getPath "smart-wallet"