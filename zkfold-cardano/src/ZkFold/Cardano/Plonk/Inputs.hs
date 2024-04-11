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
import qualified Prelude                        as Haskell

import qualified ZkFold.Base.Protocol.ARK.Plonk as ZkFold
import           ZkFold.Cardano.Plonk.Internal  (F (..), G1, G2, convertG2, convertPlonkF, powMod)

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

------------------------------------ Bench ------------------------------------

data Contract = Contract {
    x        :: ZkFold.F
  , ps       :: ZkFold.PlonkProverSecret
  , targetId :: ZkFold.F
} deriving (Show)

---------------------------------- ByteString ----------------------------------

data SetupBytes = SetupBytes {
    n'     :: Integer
  , gs'    :: [BuiltinByteString]
  , h0'    :: BuiltinByteString
  , h1'    :: BuiltinByteString
  , omega' :: Integer
  , k1'    :: Integer
  , k2'    :: Integer
  , cmQl'  :: BuiltinByteString
  , cmQr'  :: BuiltinByteString
  , cmQo'  :: BuiltinByteString
  , cmQm'  :: BuiltinByteString
  , cmQc'  :: BuiltinByteString
  , cmS1'  :: BuiltinByteString
  , cmS2'  :: BuiltinByteString
  , cmS3'  :: BuiltinByteString
} deriving (Show)

makeLift ''SetupBytes
makeIsDataIndexed ''SetupBytes [('SetupBytes,0)]

newtype InputBytes = InputBytes {
  pubInput' :: [Integer]
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

{-# INLINABLE toSetup #-}
toSetup :: SetupBytes -> SetupPlonkPlutus
toSetup SetupBytes{..} = SetupPlonkPlutus {
    n     = n'  
  , gs    = Haskell.fmap bls12_381_G1_uncompress gs' 
  , h0    = bls12_381_G2_uncompress h0' 
  , h1    = bls12_381_G2_uncompress h1' 
  , omega = F omega'
  , k1    = F k1'   
  , k2    = F k2'   
  , cmQl  = bls12_381_G1_uncompress cmQl'
  , cmQr  = bls12_381_G1_uncompress cmQr'
  , cmQo  = bls12_381_G1_uncompress cmQo'
  , cmQm  = bls12_381_G1_uncompress cmQm'
  , cmQc  = bls12_381_G1_uncompress cmQc'
  , cmS1  = bls12_381_G1_uncompress cmS1'
  , cmS2  = bls12_381_G1_uncompress cmS2'
  , cmS3  = bls12_381_G1_uncompress cmS3'
}

{-# INLINABLE toInput #-}
toInput :: InputBytes -> InputPlonkPlutus
toInput InputBytes{..} = InputPlonkPlutus {
  pubInput = Haskell.fmap F pubInput'
}

{-# INLINABLE toProof #-}
toProof :: ProofBytes -> ProofPlonkPlutus
toProof ProofBytes{..} = ProofPlonkPlutus {
    cmA    = bls12_381_G1_uncompress cmA'
  , cmB    = bls12_381_G1_uncompress cmB'
  , cmC    = bls12_381_G1_uncompress cmC'
  , cmZ    = bls12_381_G1_uncompress cmZ'
  , cmT1   = bls12_381_G1_uncompress cmT1'
  , cmT2   = bls12_381_G1_uncompress cmT2'
  , cmT3   = bls12_381_G1_uncompress cmT3'
  , proof1 = bls12_381_G1_uncompress proof1'
  , proof2 = bls12_381_G1_uncompress proof2'
  , a_xi   = F a_xi'
  , b_xi   = F b_xi'
  , c_xi   = F c_xi'
  , s1_xi  = F s1_xi'
  , s2_xi  = F s2_xi'
  , z_xi   = F z_xi'
}
