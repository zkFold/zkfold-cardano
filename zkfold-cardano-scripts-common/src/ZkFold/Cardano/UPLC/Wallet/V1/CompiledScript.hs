module ZkFold.Cardano.UPLC.Wallet.V1.CompiledScript (
  smartWalletBPFile,
) where

import Data.ByteString (ByteString)
import Data.FileEmbed
import Prelude

smartWalletBPFile :: ByteString
smartWalletBPFile = $(makeRelativeToProject "./data/compiled-scripts/smart-wallet-v1.blueprint" >>= embedFile)
