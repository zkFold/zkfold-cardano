{-# LANGUAGE TemplateHaskell #-}

module ZkFold.Cardano.Plonk.Inputs (
  SetupPlonkPlutus(..),
  InputPlonkPlutus(..),
  ProofPlonkPlutus(..),
  SetupJSON(..),
  InputJSON(..),
  ProofJSON(..),
  convertSetupPlonkPlutus,
  convertInputPlonkPlutus,
  convertProofPlonkPlutus,
) where

import           Data.Aeson                    (FromJSON, ToJSON)
import qualified Data.ByteString               as BS
import           Data.Word                     ()
import           GHC.Generics                  (Generic)
import           PlutusTx                      (makeIsDataIndexed, makeLift)
import           PlutusTx.Builtins             (BuiltinByteString, Integer, bls12_381_G1_uncompress, bls12_381_G2_scalarMul)
import qualified PlutusTx.Prelude              as P
import           Prelude                       (Functor (..), Num (..), Show, fromIntegral, reverse, ($), (.), (^))
import qualified Prelude
import qualified Prelude                       as Haskell

import           ZkFold.Cardano.Plonk.Internal (F (..), G1, G2, convertG2, powMod)

------------------------------------ Plutus ------------------------------------

data SetupPlonkPlutus = SetupPlonkPlutus {
    n     :: Integer
  , gs    :: [G1]
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
} deriving (Show)

makeLift ''SetupPlonkPlutus
makeIsDataIndexed ''SetupPlonkPlutus [('SetupPlonkPlutus,0)]

newtype InputPlonkPlutus = InputPlonkPlutus {
  pubInput :: [F]
} deriving (Show)

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
} deriving (Show)

makeLift ''ProofPlonkPlutus
makeIsDataIndexed ''ProofPlonkPlutus [('ProofPlonkPlutus,0)]

------------------------------------- JSON -------------------------------------

data SetupJSON = SetupJSON {
    n'     :: Integer
  , gs'    :: [[Integer]]
  , h0'    :: [Integer]
  , h1'    :: [Integer]
  , omega' :: [Integer]
  , k1'    :: [Integer]
  , k2'    :: [Integer]
  , cmQl'  :: [Integer]
  , cmQr'  :: [Integer]
  , cmQo'  :: [Integer]
  , cmQm'  :: [Integer]
  , cmQc'  :: [Integer]
  , cmS1'  :: [Integer]
  , cmS2'  :: [Integer]
  , cmS3'  :: [Integer]
} deriving (Show, Generic)

instance FromJSON SetupJSON
instance ToJSON SetupJSON

newtype InputJSON = InputJSON {
    pubInput' :: [Integer]
} deriving (Show, Generic)

instance FromJSON InputJSON
instance ToJSON InputJSON

data ProofJSON = ProofJSON {
    cmA'    :: [Integer]
  , cmB'    :: [Integer]
  , cmC'    :: [Integer]
  , cmZ'    :: [Integer]
  , cmT1'   :: [Integer]
  , cmT2'   :: [Integer]
  , cmT3'   :: [Integer]
  , proof1' :: [Integer]
  , proof2' :: [Integer]
  , a_xi'   :: [Integer]
  , b_xi'   :: [Integer]
  , c_xi'   :: [Integer]
  , s1_xi'  :: [Integer]
  , s2_xi'  :: [Integer]
  , z_xi'   :: [Integer]
} deriving (Show, Generic)

instance FromJSON ProofJSON
instance ToJSON ProofJSON

listToF :: [Integer] -> F
listToF = F . \[a, b, c, d] -> a + b * (2 :: Integer) ^ (64 :: Integer)
                                 + c * (2 :: Integer) ^ (128 :: Integer)
                                 + d * (2 :: Integer) ^ (192 :: Integer)

listToG2 :: [Integer] -> P.BuiltinBLS12_381_G2_Element
listToG2 = P.bls12_381_G2_uncompress . P.toBuiltin . BS.pack . Prelude.map fromIntegral

listToG1 :: [Integer] -> P.BuiltinBLS12_381_G1_Element
listToG1 = P.bls12_381_G1_uncompress . P.toBuiltin . BS.pack . Prelude.map fromIntegral

convertSetupPlonkPlutus :: SetupJSON -> SetupPlonkPlutus
convertSetupPlonkPlutus SetupJSON{..} = SetupPlonkPlutus
  { n     = n'
  , gs    = fmap listToG1 gs'
  , h0    = listToG2 h0'
  , h1    = listToG2 h1'
  , omega = listToF omega'
  , k1    = listToF k1'
  , k2    = listToF k2'
  , cmQl  = listToG1 cmQl'
  , cmQr  = listToG1 cmQr'
  , cmQo  = listToG1 cmQo'
  , cmQm  = listToG1 cmQm'
  , cmQc  = listToG1 cmQc'
  , cmS1  = listToG1 cmS1'
  , cmS2  = listToG1 cmS2'
  , cmS3  = listToG1 cmS3'
  }

convertInputPlonkPlutus :: InputJSON -> InputPlonkPlutus
convertInputPlonkPlutus InputJSON{..} = InputPlonkPlutus
  { pubInput = fmap F pubInput'
  }

convertProofPlonkPlutus :: ProofJSON -> ProofPlonkPlutus
convertProofPlonkPlutus ProofJSON{..} = ProofPlonkPlutus
  { cmA    = listToG1 cmA'
  , cmB    = listToG1 cmB'
  , cmC    = listToG1 cmC'
  , cmZ    = listToG1 cmZ'
  , cmT1   = listToG1 cmT1'
  , cmT2   = listToG1 cmT2'
  , cmT3   = listToG1 cmT3'
  , proof1 = listToG1 proof1'
  , proof2 = listToG1 proof2'
  , a_xi   = listToF a_xi'
  , b_xi   = listToF b_xi'
  , c_xi   = listToF c_xi'
  , s1_xi  = listToF s1_xi'
  , s2_xi  = listToF s2_xi'
  , z_xi   = listToF z_xi'
}
