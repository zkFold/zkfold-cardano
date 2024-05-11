{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass  #-}

module ZkFold.Cardano.Plonk.OnChain where

import           Data.Aeson                      (FromJSON, ToJSON)
import           GHC.Generics                    (Generic)
import           GHC.Natural                     (Natural, naturalToInteger)
import           PlutusTx                        (makeIsDataIndexed, makeLift)
import           PlutusTx.Builtins
import           PlutusTx.Prelude
import           Prelude                         (Show, undefined)

import qualified ZkFold.Base.Algebra.Basic.Class as ZkFold

---------------------------------- F --------------------------------------

bls12_381_field_prime :: Integer
bls12_381_field_prime = 52435875175126190479447740508185965837690552500527637822603658699938581184513

newtype F = F { toF :: Integer }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)
makeLift ''F
makeIsDataIndexed ''F [('F,0)]

instance Eq F where
    {-# INLINABLE (==) #-}
    (F a) == (F b) = a == b

instance ZkFold.AdditiveSemigroup F where
    {-# INLINABLE (+) #-}
    (F a) + (F b) = F $ (a + b) `modulo` bls12_381_field_prime

instance ZkFold.FromConstant Natural F where
    {-# INLINABLE fromConstant #-}
    fromConstant = ZkFold.fromConstant . naturalToInteger

instance ZkFold.Scale Natural F where
    {-# INLINABLE scale #-}
    scale = (ZkFold.*) . ZkFold.fromConstant

instance ZkFold.FromConstant Integer F where
    {-# INLINABLE fromConstant #-}
    fromConstant = F . (`modulo` bls12_381_field_prime)

instance ZkFold.Scale Integer F where
    {-# INLINABLE scale #-}
    scale = (ZkFold.*) . F

instance ZkFold.AdditiveMonoid F where
    {-# INLINABLE zero #-}
    zero = F 0

instance ZkFold.AdditiveGroup F where
    {-# INLINABLE (-) #-}
    (F a) - (F b) = F $ (a - b) `modulo` bls12_381_field_prime

instance ZkFold.MultiplicativeSemigroup F where
    {-# INLINABLE (*) #-}
    (F a) * (F b) = F $ (a * b) `modulo` bls12_381_field_prime

instance ZkFold.Exponent F Natural where
    {-# INLINABLE (^) #-}
    (^) f n = powMod f (naturalToInteger n)

instance ZkFold.MultiplicativeMonoid F where
    {-# INLINABLE one #-}
    one = F 1

{-# INLINABLE powMod #-}
powMod :: F -> Integer -> F
powMod b e
    | e < 0     = ZkFold.zero
    | e == 0    = ZkFold.one
    | even e    = powMod (b ZkFold.* b) (e `divide` 2)
    | otherwise = b ZkFold.* powMod (b ZkFold.* b) ((e - 1) `divide` 2)

instance ZkFold.Exponent F Integer where
    {-# INLINABLE (^) #-}
    (^) = powMod

instance ZkFold.MultiplicativeGroup F where
    {-# INLINABLE invert #-}
    invert a = powMod a (bls12_381_field_prime - 2)

    {-# INLINABLE (/) #-}
    a / b = a ZkFold.* ZkFold.invert b

---------------------------------- G1 -------------------------------------

type G1 = BuiltinBLS12_381_G1_Element

instance ZkFold.AdditiveSemigroup BuiltinBLS12_381_G1_Element where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G1_add

instance ZkFold.FromConstant Natural BuiltinBLS12_381_G1_Element where
    fromConstant = undefined

instance ZkFold.Scale Natural BuiltinBLS12_381_G1_Element where
    scale n = ZkFold.scale (naturalToInteger n)

instance ZkFold.AdditiveMonoid BuiltinBLS12_381_G1_Element where
    zero = bls12_381_G1_uncompress bls12_381_G1_compressed_zero

instance ZkFold.FromConstant Integer BuiltinBLS12_381_G1_Element where
    fromConstant = undefined

instance ZkFold.Scale Integer BuiltinBLS12_381_G1_Element where
    {-# INLINABLE scale #-}
    scale = bls12_381_G1_scalarMul

instance ZkFold.AdditiveGroup BuiltinBLS12_381_G1_Element where
    {-# INLINABLE (-) #-}
    g - h = bls12_381_G1_add g (bls12_381_G1_neg h)

{-# INLINABLE mul #-}
mul :: F -> BuiltinBLS12_381_G1_Element -> BuiltinBLS12_381_G1_Element
mul (F a) = bls12_381_G1_scalarMul a

---------------------------------- G2 -------------------------------------

type G2 = BuiltinBLS12_381_G2_Element

instance ZkFold.AdditiveSemigroup BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G2_add

instance ZkFold.FromConstant Natural BuiltinBLS12_381_G2_Element where
    fromConstant = undefined

instance ZkFold.Scale Natural BuiltinBLS12_381_G2_Element where
    scale n = ZkFold.scale (naturalToInteger n)

instance ZkFold.AdditiveMonoid BuiltinBLS12_381_G2_Element where
    {-# INLINABLE zero #-}
    zero = bls12_381_G2_uncompress bls12_381_G2_compressed_zero

instance ZkFold.FromConstant Integer BuiltinBLS12_381_G2_Element where
    fromConstant = undefined

instance ZkFold.Scale Integer BuiltinBLS12_381_G2_Element where
    {-# INLINABLE scale #-}
    scale = bls12_381_G2_scalarMul

instance ZkFold.AdditiveGroup BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (-) #-}
    g - h = bls12_381_G2_add g (bls12_381_G2_neg h)

---------------------------------- ByteString ----------------------------------

data SetupBytes = SetupBytes {
    n     :: Integer
  , g0'   :: BuiltinByteString
  , h0'   :: BuiltinByteString
  , h1'   :: BuiltinByteString
  , omega :: F
  , k1    :: F
  , k2    :: F
  , cmQl' :: BuiltinByteString
  , cmQr' :: BuiltinByteString
  , cmQo' :: BuiltinByteString
  , cmQm' :: BuiltinByteString
  , cmQc' :: BuiltinByteString
  , cmS1' :: BuiltinByteString
  , cmS2' :: BuiltinByteString
  , cmS3' :: BuiltinByteString
} deriving stock (Show, Generic)

makeLift ''SetupBytes
makeIsDataIndexed ''SetupBytes [('SetupBytes,0)]

newtype InputBytes = InputBytes {
  pubInput :: F
} deriving stock (Show, Generic)

makeLift ''InputBytes
makeIsDataIndexed ''InputBytes [('InputBytes,0)]

data ProofBytes = ProofBytes {
    cmA'    :: BuiltinByteString
  , cmB'    :: BuiltinByteString
  , cmC'    :: BuiltinByteString
  , cmZ'    :: BuiltinByteString
  , cmT1'   :: BuiltinByteString
  , cmT2'   :: BuiltinByteString
  , cmT3'   :: BuiltinByteString
  , proof1' :: BuiltinByteString
  , proof2' :: BuiltinByteString
  , a_xi'   :: Integer
  , b_xi'   :: Integer
  , c_xi'   :: Integer
  , s1_xi'  :: Integer
  , s2_xi'  :: Integer
  , z_xi'   :: Integer
  , lagsInv :: F
} deriving stock (Show, Generic)

makeLift ''ProofBytes
makeIsDataIndexed ''ProofBytes [('ProofBytes,0)]

newtype ParamsVerifier = ParamsVerifier BuiltinByteString
  deriving stock (Show, Generic)

makeLift ''ParamsVerifier
makeIsDataIndexed ''ParamsVerifier [('ParamsVerifier, 0)]

data DatumVerifier = DatumVerifier
  deriving stock (Show, Generic)

makeLift ''DatumVerifier
makeIsDataIndexed ''DatumVerifier [('DatumVerifier, 0)]

data RedeemerVerifier = RedeemerVerifier SetupBytes InputBytes ProofBytes
  deriving stock (Show, Generic)

makeLift ''RedeemerVerifier
makeIsDataIndexed ''RedeemerVerifier [('RedeemerVerifier, 0)]
