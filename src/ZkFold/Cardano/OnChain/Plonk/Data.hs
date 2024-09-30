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
    n     :: Integer
  , pow   :: Integer
  , x2'   :: BuiltinByteString
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
    cmA'       :: BuiltinByteString
  , cmB'       :: BuiltinByteString
  , cmC'       :: BuiltinByteString
  , cmZ'       :: BuiltinByteString
  , cmT1'      :: BuiltinByteString
  , cmT2'      :: BuiltinByteString
  , cmT3'      :: BuiltinByteString
  , proof1'    :: BuiltinByteString
  , proof2'    :: BuiltinByteString
  , a_xi'      :: Integer
  , b_xi'      :: Integer
  , c_xi'      :: Integer
  , s1_xi'     :: Integer
  , s2_xi'     :: Integer
  , z_xi'      :: Integer
  , l1_xi_mul' :: F
} deriving stock (Show, Generic)

makeLift ''ProofBytes
makeIsDataIndexed ''ProofBytes [('ProofBytes,0)]
