{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module ZkFold.Cardano.Plonk.Inputs where

import           Data.Aeson                    (FromJSON, ToJSON, toJSON)
import qualified Data.ByteString               as BS
import           Data.Word                     ()
import           GHC.Generics                  (Generic)
import           PlutusTx                      (makeIsDataIndexed, makeLift)
import           PlutusTx.Builtins             (BuiltinByteString, Integer, bls12_381_G1_uncompress, bls12_381_G2_scalarMul)
import qualified PlutusTx.Prelude              as P
import           Prelude                       (Functor (..), Num (..), Show, fromIntegral, reverse, ($), (.), (^))
import qualified Prelude
import qualified Prelude                       as Haskell

import           ZkFold.Cardano.Plonk.Internal (F (..), G1, G2, convertG2, powMod, convertPlonkF)
import qualified ZkFold.Base.Protocol.ARK.Plonk as ZkFold

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

data Contract = Contract {
    x        :: ZkFold.F
  , ps       :: ZkFold.PlonkProverSecret
  , targetId :: ZkFold.F
}

------------------------------------- JSON -------------------------------------

data PlonkProverSecret = PlonkProverSecret F F F F F F F F F F F
  deriving (Show, Generic, ToJSON, FromJSON)

data RowContractJSON = RowContractJSON {
    x'        :: F
  , ps'       :: PlonkProverSecret
  , targetId' :: F
} deriving (Show, Generic, ToJSON, FromJSON)

toPlonkPlonkProverSecret :: PlonkProverSecret -> ZkFold.PlonkProverSecret
toPlonkPlonkProverSecret (PlonkProverSecret b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11) =
  ZkFold.PlonkProverSecret 
    (convertPlonkF b1) 
    (convertPlonkF b2)
    (convertPlonkF b3)
    (convertPlonkF b4)
    (convertPlonkF b5)
    (convertPlonkF b6)
    (convertPlonkF b7)
    (convertPlonkF b8)
    (convertPlonkF b9)
    (convertPlonkF b10)
    (convertPlonkF b11)

toContract :: RowContractJSON -> Contract
toContract (RowContractJSON x' ps' targetId') =
  Contract (convertPlonkF x') (toPlonkPlonkProverSecret ps') (convertPlonkF targetId')
