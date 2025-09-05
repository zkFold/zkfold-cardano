module ZkFold.Cardano.OnChain.Plonkup.Update where

import PlutusTx.Builtins (
    BuiltinByteString,
    Integer,
    bls12_381_G1_add,
    bls12_381_G1_compress,
    bls12_381_G1_scalarMul,
    bls12_381_G1_uncompress,
 )
import PlutusTx.Prelude (($), (+))

import ZkFold.Algebra.Class ((*))
import ZkFold.Cardano.OnChain.Orphans ()
import ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes (..))

{-# INLINEABLE updateSetupBytes #-}
updateSetupBytes :: SetupBytes -> Integer -> BuiltinByteString -> SetupBytes
updateSetupBytes setup@SetupBytes{..} s g =
    let
        nPrv' = nPrv + 1
        omegaNPrv' = omegaNPrv * omega
        cmQc_bytes' = bls12_381_G1_compress $ bls12_381_G1_uncompress cmQc_bytes `bls12_381_G1_add` bls12_381_G1_scalarMul (-s) (bls12_381_G1_uncompress g)
     in
        setup{nPrv = nPrv', omegaNPrv = omegaNPrv', cmQc_bytes = cmQc_bytes'}
