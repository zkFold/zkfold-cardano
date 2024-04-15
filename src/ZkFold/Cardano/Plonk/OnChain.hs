{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.Plonk.OnChain where

import           Data.Aeson                               (FromJSON, ToJSON)
import           GHC.ByteOrder                            (ByteOrder (..))
import           GHC.Generics                             (Generic)
import           PlutusTx                                 (makeIsDataIndexed, makeLift)
import           PlutusTx.Builtins
import           PlutusTx.Prelude                         hiding (fromInteger)
import           Prelude                                  (Num (fromInteger), Show)
import qualified Prelude                                  as Haskell

import qualified ZkFold.Base.Algebra.Basic.Class          as ZkFold
import           ZkFold.Base.Protocol.NonInteractiveProof (FromTranscript (..), ToTranscript (..))

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

instance ZkFold.AdditiveMonoid F where
    {-# INLINABLE zero #-}
    zero = F 0

instance ZkFold.AdditiveGroup F where
    {-# INLINABLE (-) #-}
    (F a) - (F b) = F $ (a - b) `modulo` bls12_381_field_prime

instance ZkFold.MultiplicativeSemigroup F where
    {-# INLINABLE (*) #-}
    (F a) * (F b) = F $ (a * b) `modulo` bls12_381_field_prime

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

instance ZkFold.MultiplicativeGroup F where
    {-# INLINABLE invert #-}
    invert a = powMod a (bls12_381_field_prime - 2)

    {-# INLINABLE (/) #-}
    a / b = a ZkFold.* ZkFold.invert b

instance Num F where
    {-# INLINABLE (+) #-}
    (+) = (ZkFold.+)

    {-# INLINABLE (-) #-}
    (-) = (ZkFold.-)

    {-# INLINABLE (*) #-}
    (*) = (ZkFold.*)

    {-# INLINABLE negate #-}
    negate = ZkFold.negate

    {-# INLINABLE abs #-}
    abs = id

    {-# INLINABLE signum #-}
    signum = const ZkFold.one

    {-# INLINABLE fromInteger #-}
    fromInteger = F . (`modulo` bls12_381_field_prime)

---------------------------------- G1 -------------------------------------

type G1 = BuiltinBLS12_381_G1_Element

instance ZkFold.AdditiveSemigroup  BuiltinBLS12_381_G1_Element where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G1_add

instance ZkFold.AdditiveMonoid BuiltinBLS12_381_G1_Element where
    {-# INLINABLE zero #-}
    zero = bls12_381_G1_uncompress bls12_381_G1_compressed_zero

instance ZkFold.AdditiveGroup BuiltinBLS12_381_G1_Element where
    {-# INLINABLE (-) #-}
    g - h = bls12_381_G1_add g (bls12_381_G1_neg h)

mul :: F -> BuiltinBLS12_381_G1_Element -> BuiltinBLS12_381_G1_Element
mul (F a) = bls12_381_G1_scalarMul a

---------------------------------- G2 -------------------------------------

type G2 = BuiltinBLS12_381_G2_Element

instance ZkFold.AdditiveSemigroup  BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G2_add

instance ZkFold.AdditiveMonoid BuiltinBLS12_381_G2_Element where
    {-# INLINABLE zero #-}
    zero = bls12_381_G2_uncompress bls12_381_G2_compressed_zero

instance ZkFold.AdditiveGroup BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (-) #-}
    g - h = bls12_381_G2_add g (bls12_381_G2_neg h)

-------------------------- Transcript -------------------------------------

type Transcript = BuiltinByteString

instance ToTranscript BuiltinByteString F where
    {-# INLINABLE toTranscript #-}
    toTranscript f = integerToByteString BigEndian 32 $ toF f

instance ToTranscript BuiltinByteString G1 where
    {-# INLINABLE toTranscript #-}
    toTranscript = bls12_381_G1_compress

instance FromTranscript BuiltinByteString F where
    {-# INLINABLE newTranscript #-}
    newTranscript = consByteString 0

    {-# INLINABLE fromTranscript #-}
    fromTranscript = F . byteStringToInteger BigEndian . takeByteString 31 . blake2b_256

{-# INLINABLE challenge #-}
challenge :: Transcript -> (F, Transcript)
challenge ts =
    let ts' = newTranscript @BuiltinByteString @F ts
    in (fromTranscript ts', ts')

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
  , gens  :: [F]
} deriving stock (Show)

makeLift ''SetupBytes
makeIsDataIndexed ''SetupBytes [('SetupBytes,0)]

newtype InputBytes = InputBytes {
  pubInput :: [F]
} deriving stock (Show)

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
  , lagsInv :: [F]
} deriving stock (Show)

makeLift ''ProofBytes
makeIsDataIndexed ''ProofBytes [('ProofBytes,0)]
