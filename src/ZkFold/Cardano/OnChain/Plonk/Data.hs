{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.OnChain.Plonk.Data where

import           GHC.Generics                       (Generic)
import           PlutusTx                           (makeIsDataIndexed, makeLift)
import           PlutusTx.Builtins                  (BuiltinByteString, Integer)
import           Prelude                            (Show)

import           ZkFold.Cardano.OnChain.BLS12_381.F (F)

data SetupBytes = SetupBytes {
    n          :: Integer
  , pow        :: Integer
  , omega      :: F
  , k1         :: F
  , k2         :: F
  , h1_bytes   :: BuiltinByteString
  , cmQm_bytes :: BuiltinByteString
  , cmQl_bytes :: BuiltinByteString
  , cmQr_bytes :: BuiltinByteString
  , cmQo_bytes :: BuiltinByteString
  , cmQc_bytes :: BuiltinByteString
  , cmS1_bytes :: BuiltinByteString
  , cmS2_bytes :: BuiltinByteString
  , cmS3_bytes :: BuiltinByteString
} deriving stock (Show, Generic)

makeLift ''SetupBytes
makeIsDataIndexed ''SetupBytes [('SetupBytes,0)]

type InputBytes = F

data ProofBytes = ProofBytes {
    cmA_bytes     :: BuiltinByteString
  , cmB_bytes     :: BuiltinByteString
  , cmC_bytes     :: BuiltinByteString
  , cmZ1_bytes    :: BuiltinByteString
  , cmQlow_bytes  :: BuiltinByteString
  , cmQmid_bytes  :: BuiltinByteString
  , cmQhigh_bytes :: BuiltinByteString
  , proof1_bytes  :: BuiltinByteString
  , proof2_bytes  :: BuiltinByteString
  , a_xi_int      :: Integer
  , b_xi_int      :: Integer
  , c_xi_int      :: Integer
  , s1_xi_int     :: Integer
  , s2_xi_int     :: Integer
  , z1_xi'_int    :: Integer
  , l1_xi         :: F
} deriving stock (Show, Generic)

makeLift ''ProofBytes
makeIsDataIndexed ''ProofBytes [('ProofBytes,0)]
