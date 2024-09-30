module ZkFold.Cardano.OnChain.BLS12_381 (
        module ZkFold.Cardano.OnChain.BLS12_381.F,
        module ZkFold.Cardano.OnChain.BLS12_381.G1,
        module ZkFold.Cardano.OnChain.BLS12_381.G2,
        module ZkFold.Cardano.OnChain.BLS12_381
    ) where

import           PlutusTx.Builtins                   (bls12_381_G1_scalarMul)

import           ZkFold.Cardano.OnChain.BLS12_381.F
import           ZkFold.Cardano.OnChain.BLS12_381.G1
import           ZkFold.Cardano.OnChain.BLS12_381.G2

{-# INLINABLE mul #-}
mul :: F -> G1 -> G1
mul (F a) = bls12_381_G1_scalarMul a
