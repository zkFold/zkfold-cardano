module Main (main) where

import           Prelude
import           System.FilePath                          ((</>))

import           ZkFold.Cardano.UPLC.RollupSimple.Compile (writeRollupSimpleBP)
import qualified ZkFold.Cardano.UPLC.Wallet.V0.Compile    as V0
import qualified ZkFold.Cardano.UPLC.Wallet.V1.Compile    as V1

-- To be executed from root of zkfold-cardano repository.
main :: IO ()
main =
  let getPath fn = "zkfold-cardano-scripts-common" </> "data" </> "compiled-scripts" </> fn <> ".blueprint"
   in do
        V0.writeSmartWalletBP $ getPath "smart-wallet-v0"
        V1.writeSmartWalletBP $ getPath "smart-wallet-v1"
        writeRollupSimpleBP $ getPath "rollup-simple"
