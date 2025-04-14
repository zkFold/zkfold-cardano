module ZkFold.Cardano.UPLC.Wallet.CompiledScript (
  smartWalletBPFile,
) where

import Data.ByteString (ByteString)
import Data.FileEmbed
import Prelude

smartWalletBPFile :: ByteString
smartWalletBPFile = $(makeRelativeToProject "./data/compiled-scripts/smart-wallet.blueprint" >>= embedFile)
