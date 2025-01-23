{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.OffChain.Transcript where

import           Data.Word                                   (Word8)
import           PlutusTx.Builtins
import           PlutusTx.Prelude                            ((.))

import           ZkFold.Base.Algebra.Basic.Field             (toZp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (CompressedPoint)
import           ZkFold.Base.Data.ByteString                 (toByteString)
import           ZkFold.Base.Protocol.NonInteractiveProof    (FromTranscript (..), ToTranscript (..))
import           ZkFold.Cardano.OffChain.BLS12_381           (convertZp)
import           ZkFold.Cardano.OnChain.BLS12_381

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

instance ToTranscript BuiltinByteString (CompressedPoint BLS12_381_G1) where
    toTranscript = toBuiltin . toByteString

instance FromTranscript BuiltinByteString Fr where
    fromTranscript = toZp . byteStringToInteger LittleEndian . blake2b_224
