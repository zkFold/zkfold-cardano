module ZkFold.Cardano.UPLC.Wallet.CompiledScript (
  smartWalletBPFileName,
) where

import Paths_zkfold_cardano_scripts_common (getDataFileName)
import Prelude

smartWalletBPFileName :: IO FilePath
smartWalletBPFileName = getDataFileName "compiled-scripts/smart-wallet.blueprint"
