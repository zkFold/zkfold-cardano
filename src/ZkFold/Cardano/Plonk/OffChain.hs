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
import qualified Prelude                                     as Haskell

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field             (Ext2 (..), Zp, fromZp, toZp)
import           ZkFold.Base.Algebra.Basic.Number            (KnownNat, value)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, BLS12_381_G2, Fr)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (Point (..))
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
  , x2'   = convertG2 x2''
  , omega = F $ convertF omega''
  , k1    = F $ convertF k1''
  , k2    = F $ convertF k2''
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
mkInput (PlonkInput input) = InputBytes . F . convertF $ V.head input

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
  , a_xi'   = convertF a_xi
  , b_xi'   = convertF b_xi
  , c_xi'   = convertF c_xi
  , s1_xi'  = convertF s1_xi
  , s2_xi'  = convertF s2_xi
  , z_xi'   = convertF z_xi
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

convertF :: Fr -> Integer
convertF = naturalToInteger . fromZp

convertPlonkF :: F -> Fr
convertPlonkF (F n) = toZp n

convertZp :: Zp p -> Integer
convertZp = naturalToInteger . fromZp

-- See CIP-0381 for the conversion specification
convertG1 :: Point BLS12_381_G1 -> BuiltinByteString
convertG1 Inf = bls12_381_G1_compressed_zero
convertG1 (Point x y) = bs
    where
        bsX = integerToByteString BigEndian 48 $ convertZp x
        b   = indexByteString bsX 0
        b'  = b + 128 + 32 * (if y Haskell.> negate y then 1 else 0)
        bs  = consByteString b' $ sliceByteString 1 47 bsX

-- See CIP-0381 for the conversion specification
convertG2 :: Point BLS12_381_G2 -> BuiltinByteString
convertG2 Inf = bls12_381_G2_compressed_zero
convertG2 (Point x y) = bs
    where
        f (Ext2 a0 a1) = integerToByteString BigEndian 48 (convertZp a1) <> integerToByteString BigEndian 48 (convertZp a0)
        bsX  = f x
        bsY  = f y
        bsY' = f $ negate y
        b   = indexByteString bsX 0
        b'  = b + 128 + 32 * (if bsY `greaterThanByteString` bsY' then 1 else 0)
        bs  = consByteString b' $ sliceByteString 1 95 bsX

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

------------------------------------ Bench -------------------------------------

-- TODO: Refactor it. Name `Contract` does not make much sense. This type can only be used for testing.
data Contract = Contract {
    x           :: Fr
  , ps          :: PlonkProverSecret BLS12_381_G1
  , targetValue :: Fr
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ToJSON (PlonkProverSecret BLS12_381_G1)
deriving anyclass instance FromJSON (PlonkProverSecret BLS12_381_G1)
