{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.OffChain.Transcript where

import           Data.Word                              (Word8)
import           PlutusTx.Builtins                      (BuiltinByteString, ByteOrder (..), blake2b_224,
                                                         bls12_381_G1_compress, byteStringToInteger,
                                                         integerToByteString, toBuiltin)
import           PlutusTx.Prelude                       ((.))

import           ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_CompressedPoint, Fr)
import           ZkFold.Algebra.Field                   (toZp)
import           ZkFold.Cardano.OffChain.BLS12_381      (convertZp)
import           ZkFold.Cardano.OnChain.BLS12_381.F     (F (..))
import           ZkFold.Cardano.OnChain.BLS12_381.G1    (G1)
import           ZkFold.Data.ByteString                 (toByteString)
import           ZkFold.Protocol.NonInteractiveProof    (FromTranscript (..), ToTranscript (..))

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

instance ToTranscript BuiltinByteString BLS12_381_G1_CompressedPoint where
    toTranscript = toBuiltin . toByteString

instance FromTranscript BuiltinByteString Fr where
    fromTranscript = toZp . byteStringToInteger LittleEndian . blake2b_224
