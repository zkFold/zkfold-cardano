{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.Plonk.OffChain where

import           Data.Aeson                                  (FromJSON, ToJSON)
import qualified Data.Vector                                 as V
import           GHC.ByteOrder                               (ByteOrder (..))
import           GHC.Generics                                (Generic)
import           GHC.Natural                                 (naturalToInteger)
import           PlutusTx.Builtins
import           PlutusTx.Prelude                            (Semigroup (..), ($), (.))
import           Prelude                                     (Show)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field             (Zp, fromZp, toZp)
import           ZkFold.Base.Algebra.Basic.Number            (KnownNat, value)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, BLS12_381_G2, Fr)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (Point (..), EllipticCurve (..), compress)
import           ZkFold.Base.Data.ByteString                 (toByteString)
import           ZkFold.Base.Protocol.ARK.Plonk
import           ZkFold.Base.Protocol.NonInteractiveProof    (FromTranscript (..), NonInteractiveProof (..), ToTranscript (..))
import           ZkFold.Cardano.Plonk.OnChain.BLS12_381.F    (F (..))
import           ZkFold.Cardano.Plonk.OnChain.BLS12_381.G1   (G1)
import           ZkFold.Cardano.Plonk.OnChain.Data           (InputBytes (..), ProofBytes (..), SetupBytes (..))

--------------- Transform Plonk Base to Plonk BuiltinByteString ----------------

type PlonkN n = Plonk n 1 BLS12_381_G1 BLS12_381_G2 BuiltinByteString

mkSetup :: SetupVerify (PlonkN n) -> SetupBytes
mkSetup (PlonkSetupParamsVerify {..}, PlonkCircuitCommitments {..}) = SetupBytes
  { n     = n''
  , pow   = pow''
  , g0'   = convertG1 gen
  , h0'   = convertG2 gen
  , x2'   = convertG2 x2''
  , omega = F $ convertZp omega''
  , k1    = F $ convertZp k1''
  , k2    = F $ convertZp k2''
  , cmQl' = convertG1 cmQl
  , cmQr' = convertG1 cmQr
  , cmQo' = convertG1 cmQo
  , cmQm' = convertG1 cmQm
  , cmQc' = convertG1 cmQc
  , cmS1' = convertG1 cmS1
  , cmS2' = convertG1 cmS2
  , cmS3' = convertG1 cmS3
  }

mkInput :: Input (PlonkN n) -> InputBytes
mkInput (PlonkInput input) = InputBytes . F . convertZp $ V.head input

mkProof :: forall n. KnownNat n => SetupBytes -> Proof (PlonkN n) -> ProofBytes
mkProof setup' proof@(PlonkProof cmA cmB cmC cmZ cmT1 cmT2 cmT3 proof1 proof2 a_xi b_xi c_xi s1_xi s2_xi z_xi) = ProofBytes
  { cmA'    = convertG1 cmA
  , cmB'    = convertG1 cmB
  , cmC'    = convertG1 cmC
  , cmZ'    = convertG1 cmZ
  , cmT1'   = convertG1 cmT1
  , cmT2'   = convertG1 cmT2
  , cmT3'   = convertG1 cmT3
  , proof1' = convertG1 proof1
  , proof2' = convertG1 proof2
  , a_xi'   = convertZp a_xi
  , b_xi'   = convertZp b_xi
  , c_xi'   = convertZp c_xi
  , s1_xi'  = convertZp s1_xi
  , s2_xi'  = convertZp s2_xi
  , z_xi'   = convertZp z_xi
  , lagsInv = lagrangesInvariants @n setup' proof
  }

lagrangesInvariants :: forall n. KnownNat n => SetupBytes -> Proof (PlonkN n) -> F
lagrangesInvariants  SetupBytes{omega} (PlonkProof cmA cmB cmC cmZ cmT1 cmT2 cmT3 _ _ _ _ _ _ _ _) =
    let (_, ts) = challenge $ convertG1 cmA <> convertG1 cmB <> convertG1 cmC
        (_, t1) = challenge ts
        (_, t2) = challenge $ t1 <> convertG1 cmZ
        (xi, _) = challenge $ t2 <> convertG1 cmT1 <> convertG1 cmT2 <> convertG1 cmT3
    in ((/) one . (\x -> F (naturalToInteger $ value @n) * (xi - x))) omega

challenge :: BuiltinByteString -> (F, BuiltinByteString)
challenge ts =
    let ts' = newTranscript @BuiltinByteString @F ts
    in (fromTranscript ts', ts')

------------------------------- Base Conversions -------------------------------

convertPlonkF :: F -> Fr
convertPlonkF (F n) = toZp n

convertZp :: Zp p -> Integer
convertZp = naturalToInteger . fromZp

-- See CIP-0381 for the conversion specification
convertG1 :: Point BLS12_381_G1 -> BuiltinByteString
convertG1 = toBuiltin . toByteString . compress

-- See CIP-0381 for the conversion specification
convertG2 :: Point BLS12_381_G2 -> BuiltinByteString
convertG2 = toBuiltin . toByteString . compress

------------------ Transcript for NonInteractiveProof Plonk32 ------------------

instance ToTranscript BuiltinByteString F where
    toTranscript (F n) = integerToByteString LittleEndian 32 n

instance ToTranscript BuiltinByteString G1 where
    toTranscript = bls12_381_G1_compress

instance FromTranscript BuiltinByteString F where
    newTranscript = consByteString 0

    fromTranscript = F . byteStringToInteger LittleEndian . blake2b_224

instance ToTranscript BuiltinByteString Fr where
    toTranscript = integerToByteString LittleEndian 32 . convertZp

instance ToTranscript BuiltinByteString (Point BLS12_381_G1) where
    toTranscript = convertG1

instance FromTranscript BuiltinByteString Fr where
    newTranscript = consByteString 0

    fromTranscript = toZp . byteStringToInteger LittleEndian . blake2b_224

------------------------------------ E2E test -------------------------------------

-- TODO: Refactor it. Name `Contract` does not make much sense. This type can only be used for testing.
data Contract = Contract {
    x           :: Fr
  , ps          :: PlonkProverSecret BLS12_381_G1
  , targetValue :: Fr
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ToJSON (PlonkProverSecret BLS12_381_G1)
deriving anyclass instance FromJSON (PlonkProverSecret BLS12_381_G1)