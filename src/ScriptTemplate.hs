module ScriptTemplate where

import           ZkFold.Symbolic.Data.Bool       (Bool (..))
import           ZkFold.Symbolic.Data.Eq         (Eq(..))
import           ZkFold.Symbolic.Types           (Symbolic)

scriptTemplate :: forall a . Symbolic a => a -> a -> Bool a
scriptTemplate x y = x == y