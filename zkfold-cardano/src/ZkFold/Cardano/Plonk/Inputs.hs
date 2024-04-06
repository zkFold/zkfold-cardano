{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

module ZkFold.Cardano.Plonk.Inputs where

import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.ByteString               as BS (pack)
import           Data.Word                     ()
import           GHC.Generics                  (Generic)
import           PlutusTx                      (makeIsDataIndexed, makeLift)
import           PlutusTx.Builtins             (Integer)
import qualified PlutusTx.Prelude              as P
import           Prelude                       (Num (..), Show, fromIntegral, ($), (.), (^))
import qualified Prelude
import qualified Prelude                       as Haskell

import           ZkFold.Cardano.Plonk.Internal (F (..), G1, G2)

data SetupPlonkPlutus = SetupPlonkPlutus {
    n     :: Integer
  , g0    :: G1
  , h0    :: G2
  , h1    :: G2
  , omega :: F
  , k1    :: F
  , k2    :: F
  , cmQl  :: G1
  , cmQr  :: G1
  , cmQo  :: G1
  , cmQm  :: G1
  , cmQc  :: G1
  , cmS1  :: G1
  , cmS2  :: G1
  , cmS3  :: G1
} deriving (Haskell.Show)

makeLift ''SetupPlonkPlutus
makeIsDataIndexed ''SetupPlonkPlutus [('SetupPlonkPlutus,0)]

newtype InputPlonkPlutus = InputPlonkPlutus { pubInput :: F } deriving (Haskell.Show)

makeLift ''InputPlonkPlutus
makeIsDataIndexed ''InputPlonkPlutus [('InputPlonkPlutus,0)]

data ProofPlonkPlutus = ProofPlonkPlutus {
    cmA    :: G1
  , cmB    :: G1
  , cmC    :: G1
  , cmZ    :: G1
  , cmT1   :: G1
  , cmT2   :: G1
  , cmT3   :: G1
  , proof1 :: G1
  , proof2 :: G1
  , a_xi   :: F
  , b_xi   :: F
  , c_xi   :: F
  , s1_xi  :: F
  , s2_xi  :: F
  , z_xi   :: F
} deriving (Haskell.Show)

makeLift ''ProofPlonkPlutus
makeIsDataIndexed ''ProofPlonkPlutus [('ProofPlonkPlutus,0)]


-- Create a quick type for importing a test vector PreInputs via JSON.
data SetupJSON = SetupJSON
    { n_public      :: Integer -- unused
    , pow           :: Integer
    , k_1           :: [Integer]
    , k_2           :: [Integer]
    , q_m           :: [Integer]
    , q_l           :: [Integer]
    , q_r           :: [Integer]
    , q_o           :: [Integer]
    , q_c           :: [Integer]
    , s_sig1_pre_in :: [Integer]
    , s_sig2_pre_in :: [Integer]
    , s_sig3_pre_in :: [Integer]
    , x_2           :: [Integer]
    , gen           :: [Integer]
} deriving (Show, Generic)

instance FromJSON SetupJSON
instance ToJSON SetupJSON

newtype InputJSON = InputJSON {
    pubInputJ :: Integer
} deriving (Show, Generic)

instance FromJSON InputJSON
instance ToJSON InputJSON

-- Create a quick type for importing a test vector Proof via JSON.
data ProofJSON = ProofJSON
    { commitment_a :: [Integer]
    , commitment_b :: [Integer]
    , commitment_c :: [Integer]
    , commitment_z :: [Integer]
    , t_low        :: [Integer]
    , t_mid        :: [Integer]
    , t_high       :: [Integer]
    , w_omega      :: [Integer]
    , w_omega_zeta :: [Integer]
    , a_eval       :: [Integer]
    , b_eval       :: [Integer]
    , c_eval       :: [Integer]
    , s_sig1       :: [Integer]
    , s_sig2       :: [Integer]
    , z_omega      :: [Integer]
} deriving (Show, Generic)

instance FromJSON ProofJSON
instance ToJSON ProofJSON

convertIntegersByteString :: [Integer] -> P.BuiltinByteString
convertIntegersByteString n = P.toBuiltin . BS.pack $ Prelude.map fromIntegral n

convertIntegersByteStringG2 :: [Integer] -> P.BuiltinByteString
convertIntegersByteStringG2 n = P.toBuiltin . BS.pack $ Prelude.map fromIntegral n

convertMontgomery :: [Integer] -> Integer
convertMontgomery [a, b, c, d] = a + b * (2 :: Integer) ^ (64 :: Integer)
                                   + c * (2 :: Integer) ^ (128 :: Integer)
                                   + d * (2 :: Integer) ^ (192 :: Integer)
convertMontgomery _            = 0

listToF :: [Integer] -> F
listToF = F . convertMontgomery

listToG2 :: [Integer] -> P.BuiltinBLS12_381_G2_Element
listToG2 = P.bls12_381_G2_uncompress . convertIntegersByteStringG2

listToG1 :: [Integer] -> P.BuiltinBLS12_381_G1_Element
listToG1 = P.bls12_381_G1_uncompress . convertIntegersByteString

convertSetupPlonkPlutus :: SetupJSON -> SetupPlonkPlutus
convertSetupPlonkPlutus SetupJSON{..} = SetupPlonkPlutus
  { n   = pow
  , g0  = P.bls12_381_G1_uncompress P.bls12_381_G1_compressed_generator
  , h0  = P.bls12_381_G2_uncompress P.bls12_381_G2_compressed_generator
  , h1  = listToG2 x_2
  , omega = listToF gen
  , k1    = listToF k_1
  , k2    = listToF k_2
  , cmQl  = listToG1 q_l
  , cmQr  = listToG1 q_r
  , cmQo  = listToG1 q_o
  , cmQm  = listToG1 q_m
  , cmQc  = listToG1 q_c
  , cmS1  = listToG1 s_sig1_pre_in
  , cmS2  = listToG1 s_sig2_pre_in
  , cmS3  = listToG1 s_sig3_pre_in
  }

convertInputPlonkPlutus :: InputJSON -> InputPlonkPlutus
convertInputPlonkPlutus InputJSON{..} = InputPlonkPlutus
  { pubInput = F pubInputJ
  }

convertProofPlonkPlutus :: ProofJSON -> ProofPlonkPlutus
convertProofPlonkPlutus ProofJSON{..} = ProofPlonkPlutus
  { cmA    = listToG1 commitment_a
  , cmB    = listToG1 commitment_b
  , cmC    = listToG1 commitment_c
  , cmZ    = listToG1 commitment_z
  , cmT1   = listToG1 t_low
  , cmT2   = listToG1 t_mid
  , cmT3   = listToG1 t_high
  , proof1 = listToG1 w_omega
  , proof2 = listToG1 w_omega_zeta
  , a_xi   = listToF  a_eval
  , b_xi   = listToF  b_eval
  , c_xi   = listToF  c_eval
  , s1_xi  = listToF  s_sig1
  , s2_xi  = listToF  s_sig2
  , z_xi   = listToF  z_omega
}
