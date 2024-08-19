{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module ZkFold.Cardano.Plonk.OnChain.BLS12_381.G1 where

import           GHC.Natural                     (Natural, naturalToInteger)
import           PlutusTx.Builtins
import           PlutusTx.Prelude                (traceError)

import qualified ZkFold.Base.Algebra.Basic.Class as ZkFold

type G1 = BuiltinBLS12_381_G1_Element

instance ZkFold.AdditiveSemigroup G1 where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G1_add

instance ZkFold.FromConstant Natural G1 where
    fromConstant = traceError "not supported on on-chain"

instance ZkFold.Scale Natural G1 where
    scale n = ZkFold.scale (naturalToInteger n)

instance ZkFold.AdditiveMonoid G1 where
    zero = bls12_381_G1_uncompress bls12_381_G1_compressed_zero

instance ZkFold.FromConstant Integer G1 where
    fromConstant = traceError "not supported on on-chain"

instance ZkFold.Scale Integer G1 where
    {-# INLINABLE scale #-}
    scale = bls12_381_G1_scalarMul

instance ZkFold.AdditiveGroup G1 where
    {-# INLINABLE (-) #-}
    g - h = bls12_381_G1_add g (bls12_381_G1_neg h)
