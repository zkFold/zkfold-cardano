module ZkFold.Cardano.UPLC.RollupSimple.CompiledScript (
  rollupSimpleBPFile,
) where

import           Data.ByteString (ByteString)
import           Data.FileEmbed
import           Prelude

rollupSimpleBPFile :: ByteString
rollupSimpleBPFile = $(makeRelativeToProject "./data/compiled-scripts/rollup-simple.blueprint" >>= embedFile)
