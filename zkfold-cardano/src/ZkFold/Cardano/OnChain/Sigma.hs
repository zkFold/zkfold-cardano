{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.OnChain.Sigma where

import           GHC.Generics          (Generic)
import Data.Binary (Binary (..))
import Data.Foldable (Foldable)
import Data.Functor.Rep (Rep, Representable)

import PlutusTx.Builtins (
    BuiltinByteString,
    ByteOrder (..),
    addInteger,
    blake2b_224,
    bls12_381_G1_compressed_zero,
    bls12_381_G1_compressed_generator,
    bls12_381_G1_uncompress,
    bls12_381_G2_compressed_generator,
    bls12_381_G2_uncompress,
    bls12_381_finalVerify,
    bls12_381_millerLoop,
    byteStringToInteger,
    consByteString,
    emptyByteString,
    integerToByteString,
    subtractInteger,
 )
import PlutusTx.Prelude (Bool (..), Maybe (..), Eq(..), Integer, map, length, zipWith, foldl, (!!), ($), (&&), (.), (<>), (==))
import           PlutusTx              (makeLift)
import           PlutusTx.Blueprint
import PlutusTx.AssocMap
import PlutusTx.Blueprint.TH qualified
import Prelude (undefined)
import Prelude qualified as Haskell

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (KnownNat, type (*), type (+))
import ZkFold.Cardano.OffChain.Transcript ()
import ZkFold.Cardano.OnChain.BLS12_381 (F (..), mul, powTwo, G1)
import ZkFold.Protocol.NonInteractiveProof (CompatibleNonInteractiveProofs (..), NonInteractiveProof (..))

newtype ScalarVariable = Var Integer
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (HasBlueprintDefinition, Eq)
makeLift ''ScalarVariable

data LinearCombination = 
    LinearCombination 
        { scalarVariables :: [ScalarVariable]
        , elements :: [G1]
        }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (HasBlueprintDefinition)
makeLift ''LinearCombination

data Constraint = 
    Constraint
        { lhs :: G1
        , rhs :: [(ScalarVariable, G1)] 
        }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (HasBlueprintDefinition)
makeLift ''Constraint

{-# INLINEABLE applyLinearCombination #-}
applyLinearCombination :: Map ScalarVariable F -> LinearCombination -> G1
applyLinearCombination scalarValues LinearCombination{..} = foldl (+) z $ zipWith scale scalars elements 
  where
    z = bls12_381_G1_uncompress bls12_381_G1_compressed_zero
    scalars = map (\v -> let Just (F s) = lookup v scalarValues in s) scalarVariables


{-# INLINEABLE linearMap #-}
linearMap :: Map ScalarVariable F -> [LinearCombination] -> [G1]
linearMap scalarValues = map (applyLinearCombination scalarValues)

data PlonkupSigma

instance NonInteractiveProof PlonkupSigma where
    type Transcript PlonkupSigma = () 
    type SetupProve PlonkupSigma = ()
    type SetupVerify PlonkupSigma = ([G1], F)
    type Witness PlonkupSigma = ()
    type Input PlonkupSigma = ()
    type Proof PlonkupSigma = [F]

    setupProve :: PlonkupSigma -> SetupProve PlonkupSigma
    setupProve = undefined

    setupVerify :: PlonkupSigma -> SetupVerify PlonkupSigma
    setupVerify = undefined

    prove :: SetupProve PlonkupSigma -> Witness PlonkupSigma -> (Input PlonkupSigma, Proof PlonkupSigma)
    prove = undefined

    {-# INLINEABLE verify #-}
    verify :: SetupVerify PlonkupSigma -> Input PlonkupSigma -> Proof PlonkupSigma -> Bool
    verify (commitment, challenge) _ response = expected == got
      where
        g = bls12_381_G1_uncompress bls12_381_G1_compressed_generator
        expected = map (\r -> scale r g) response
        got = []

{--
instance
    ( Representable i
    , Representable o
    , Foldable o
    , Binary (Rep i)
    , KnownNat n
    , KnownNat (4 * n + 6)
    ) =>
    CompatibleNonInteractiveProofs (PlonkupN i o n) PlonkupSigma
    where
    nipSetupTransform = mkSetup
    nipInputTransform = mkInput
    nipProofTransform = mkProof
--}
