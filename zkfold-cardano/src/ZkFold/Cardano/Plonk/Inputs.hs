{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module ZkFold.Cardano.Plonk.Inputs where

import           Data.Aeson                     (FromJSON, ToJSON, toJSON)
import qualified Data.ByteString                as BS
import           Data.Word                      ()
import           GHC.Generics                   (Generic)
import           PlutusTx                       (makeIsDataIndexed, makeLift)
import           PlutusTx.Builtins              (BuiltinByteString, Integer, bls12_381_G1_uncompress, bls12_381_G2_uncompress)
import qualified PlutusTx.Prelude               as P
import           Prelude                        (Functor (..), Num (..), Show, fromIntegral, reverse, ($), (.), (^))
import qualified Prelude

import qualified ZkFold.Base.Protocol.ARK.Plonk as ZkFold
import           ZkFold.Cardano.Plonk.Internal  (F (..), G1, G2, convertG2, convertPlonkF, powMod)

------------------------------------ Bench ------------------------------------

data Contract = Contract {
    x        :: ZkFold.F
  , ps       :: ZkFold.PlonkProverSecret
  , targetId :: ZkFold.F
} deriving (Show)

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
} deriving (Show)

makeLift ''SetupBytes
makeIsDataIndexed ''SetupBytes [('SetupBytes,0)]

newtype InputBytes = InputBytes {
  pubInput :: [F]
} deriving (Show)

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
} deriving (Show)

makeLift ''ProofBytes
makeIsDataIndexed ''ProofBytes [('ProofBytes,0)]

------------------------------------- JSON -------------------------------------

data PlonkProverSecret = PlonkProverSecret F F F F F F F F F F F
  deriving (Show, Generic, ToJSON, FromJSON)

data RowContractJSON = RowContractJSON {
    x'        :: F
  , ps'       :: PlonkProverSecret
  , targetId' :: F
} deriving (Show, Generic, ToJSON, FromJSON)

------------------------------------- Utils -------------------------------------

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
