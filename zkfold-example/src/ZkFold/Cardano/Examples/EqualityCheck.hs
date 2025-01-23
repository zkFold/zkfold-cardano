module ZkFold.Cardano.Examples.EqualityCheck where

import           Prelude                           hiding (Bool, Eq (..), Fractional (..), Num (..), length)

import           ZkFold.Base.Algebra.Basic.Class   (FromConstant (..))
import           ZkFold.Symbolic.Class             (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool         (Bool (..))
import           ZkFold.Symbolic.Data.Eq           (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement (FieldElement)

equalityCheckContract :: forall a c . (Symbolic c, FromConstant a (BaseField c)) => a -> FieldElement c -> Bool c
equalityCheckContract targetValue inputValue = inputValue == fromConstant targetValue
