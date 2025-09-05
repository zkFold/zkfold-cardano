{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.OnChain.BLS12_381.G2 where

import GHC.Natural (Natural, naturalToInteger)
import PlutusTx.Builtins
import PlutusTx.Prelude (traceError)

import ZkFold.Algebra.Class qualified as ZkFold

type G2 = BuiltinBLS12_381_G2_Element

instance ZkFold.AdditiveSemigroup G2 where
    {-# INLINEABLE (+) #-}
    (+) = bls12_381_G2_add

instance ZkFold.FromConstant Natural G2 where
    fromConstant = traceError "not supported on on-chain"

instance ZkFold.Scale Natural G2 where
    scale n = ZkFold.scale (naturalToInteger n)

instance ZkFold.Zero G2 where
    {-# INLINEABLE zero #-}
    zero = bls12_381_G2_uncompress bls12_381_G2_compressed_zero

instance ZkFold.AdditiveMonoid G2

instance ZkFold.FromConstant Integer G2 where
    fromConstant = traceError "not supported on on-chain"

instance ZkFold.Scale Integer G2 where
    {-# INLINEABLE scale #-}
    scale = bls12_381_G2_scalarMul

instance ZkFold.AdditiveGroup G2 where
    {-# INLINEABLE (-) #-}
    g - h = bls12_381_G2_add g (bls12_381_G2_neg h)
