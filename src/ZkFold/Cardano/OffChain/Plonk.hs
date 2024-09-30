{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.OffChain.Plonk where

import           Data.Aeson                                        (FromJSON, ToJSON)
import           Data.Word                                         (Word8)
import           GHC.ByteOrder                                     (ByteOrder (..))
import           GHC.Generics                                      (Generic)
import           GHC.Natural                                       (naturalToInteger)
import           PlutusTx.Builtins
import           PlutusTx.Prelude                                  (($), (.))
import           Prelude                                           (Show, fromIntegral)

import           ZkFold.Base.Algebra.Basic.Field                   (Zp, fromZp, toZp)
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381       (BLS12_381_G1, BLS12_381_G2, Fr)
import           ZkFold.Base.Algebra.EllipticCurve.Class           (Point (..), PointCompressed, compress)
import           ZkFold.Base.Data.ByteString                       (toByteString)
import qualified ZkFold.Base.Data.Vector                           as V
import           ZkFold.Base.Protocol.NonInteractiveProof          (FromTranscript (..), NonInteractiveProof (..),
                                                                    ToTranscript (..))
import           ZkFold.Base.Protocol.Plonk
import           ZkFold.Base.Protocol.Plonkup.Input
import           ZkFold.Base.Protocol.Plonkup.Proof
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret
import           ZkFold.Base.Protocol.Plonkup.Verifier.Commitments
import           ZkFold.Base.Protocol.Plonkup.Verifier.Setup
import           ZkFold.Cardano.OnChain.BLS12_381
import           ZkFold.Cardano.OnChain.Plonk.Data                 (InputBytes (..), ProofBytes (..), SetupBytes (..))
import           ZkFold.Prelude                                    (log2ceiling)

--------------- Transform Plonk Base to Plonk BuiltinByteString ----------------

type PlonkN i n = Plonk i n 1 BLS12_381_G1 BLS12_381_G2 BuiltinByteString

mkSetup :: forall i n . KnownNat n => SetupVerify (PlonkN i n) -> SetupBytes
mkSetup PlonkupVerifierSetup {..} =
  let PlonkupCircuitCommitments {..} = commitments
  in SetupBytes
    { n     = fromIntegral (value @n)
    , pow   = log2ceiling (value @n)
    , x2'   = convertG2 h1
    , omega = F $ convertZp omega
    , k1    = F $ convertZp k1
    , k2    = F $ convertZp k2
    , cmQm' = convertG1 cmQm
    , cmQl' = convertG1 cmQl
    , cmQr' = convertG1 cmQr
    , cmQo' = convertG1 cmQo
    , cmQc' = convertG1 cmQc
    , cmS1' = convertG1 cmS1
    , cmS2' = convertG1 cmS2
    , cmS3' = convertG1 cmS3
    }

mkInput :: Input (PlonkN i n) -> InputBytes
mkInput (PlonkupInput input) = InputBytes . F . convertZp $ V.head input

mkProof :: Proof (PlonkN i n) -> ProofBytes
mkProof PlonkupProof {..} = ProofBytes
  { cmA'       = convertG1 cmA
  , cmB'       = convertG1 cmB
  , cmC'       = convertG1 cmC
  , cmZ'       = convertG1 cmZ1
  , cmT1'      = convertG1 cmQlow
  , cmT2'      = convertG1 cmQmid
  , cmT3'      = convertG1 cmQhigh
  , proof1'    = convertG1 proof1
  , proof2'    = convertG1 proof2
  , a_xi'      = convertZp a_xi
  , b_xi'      = convertZp b_xi
  , c_xi'      = convertZp c_xi
  , s1_xi'     = convertZp s1_xi
  , s2_xi'     = convertZp s2_xi
  , z_xi'      = convertZp z1_xi'
  , l1_xi_mul' = F $ convertZp l1_xi
  }

------------------------------- Base Conversions -------------------------------

convertZp :: Zp p -> Integer
convertZp = naturalToInteger . fromZp

convertG1 :: Point BLS12_381_G1 -> BuiltinByteString
convertG1 = toBuiltin . toByteString . compress

convertG2 :: Point BLS12_381_G2 -> BuiltinByteString
convertG2 = toBuiltin . toByteString . compress

------------------ Transcript for NonInteractiveProof Plonk32 ------------------

instance ToTranscript BuiltinByteString Word8 where
    toTranscript = toBuiltin . toByteString

instance ToTranscript BuiltinByteString F where
    toTranscript (F n) = integerToByteString LittleEndian 32 n

instance ToTranscript BuiltinByteString G1 where
    toTranscript = bls12_381_G1_compress

instance FromTranscript BuiltinByteString F where
    fromTranscript = F . byteStringToInteger LittleEndian . blake2b_224

instance ToTranscript BuiltinByteString Fr where
    toTranscript = integerToByteString LittleEndian 32 . convertZp

instance ToTranscript BuiltinByteString (PointCompressed BLS12_381_G1) where
    toTranscript = toBuiltin . toByteString

instance FromTranscript BuiltinByteString Fr where
    fromTranscript = toZp . byteStringToInteger LittleEndian . blake2b_224

------------------------------------ E2E test -------------------------------------

-- This type can only be used for testing.
data EqualityCheckContract = EqualityCheckContract {
    x           :: Fr
  , ps          :: PlonkupProverSecret BLS12_381_G1
  , targetValue :: Fr
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance FromJSON (V.Vector 19 Fr)

deriving anyclass instance ToJSON   (PlonkupProverSecret BLS12_381_G1)
deriving anyclass instance FromJSON (PlonkupProverSecret BLS12_381_G1)
