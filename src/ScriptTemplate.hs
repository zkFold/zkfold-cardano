module ScriptTemplate where

import           ZkFold.Symbolic.Data.Bool       (Bool (..))
import           ZkFold.Symbolic.Data.Eq         (Eq(..))
import           ZkFold.Symbolic.Types           (Symbolic)

-- Edit this script to test the zkFold Symbolic compiler
scriptTemplate :: forall a . Symbolic a => a -> a -> Bool a
scriptTemplate x y = x == y