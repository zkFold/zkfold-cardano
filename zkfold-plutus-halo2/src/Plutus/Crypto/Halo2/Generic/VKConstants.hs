{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Plutus.Crypto.Halo2.Generic.VKConstants (
  s_g2_val,
  omega_val,
  omegaInv_val,
  barycentricWeight_val,
  transcriptRepr,
  blinding_factors,
  f1_commitment,
  f2_commitment,
  f3_commitment,
  f4_commitment,
  f5_commitment,
  f6_commitment,
  f7_commitment,
  f8_commitment,
  f9_commitment,

  p1_commitment,
  p2_commitment,
  p3_commitment,
  p4_commitment,

) where

import Data.Bifunctor (bimap)
import Plutus.Crypto.BlsTypes (
  Scalar,
  bls12_381_field_prime,
  mkFp,
  mkScalar,
 )
import Plutus.Crypto.Halo2.CompressUncompress (
  constructG1Point,
 )
import PlutusTx.Prelude (
  BuiltinBLS12_381_G1_Element,
  BuiltinBLS12_381_G2_Element,
  bls12_381_G1_compressed_zero,
  bls12_381_G1_uncompress,
  bls12_381_G2_uncompress,
  BuiltinByteString,
  modulo,
 )
import PlutusTx.Builtins.HasOpaque (
  stringToBuiltinByteStringHex,
 )

f_commitments_val_pairs :: [(Integer, Integer)]
f_commitments_val_pairs =
  [
    (0x107b52dd977e260f66d3b65306dfc785a86da39fcf6f26a7404159646ddf23e96b96f1cd40f0ad7771d5bfceedab41df, 0x114ba8d5e1960f277aedbae8670a572ace357e96d61ad2c3298f3e2a5362125dbd0be7bf4dfe40c691fc0de59fcb5cca),
    (0x18b82e240007463f803aaab48bacafe9259ae97362d9251ba8e3979581f9a77e5d65d20bcbe98f781cc16e1886d97cb3, 0x0bb64eaab270a8a311eef3e8cf613fd69509c53b83d7cb1abfe7406caf00b18822dc398e4cc506bada66ca9d25a8929c),
    (0x042e83a6931a49b9fe1bbbf1ecea562cc4b0e78db537c98f9419163df3a6dc5187e7313be9e7364c76761b7730d21304, 0x0edaebb166c6980dbe8d4a189b0bdf51edb77a492345beb0e9bb7294a7e210c66e4cbbdba27c619b910fa78eb253219d),
    (0x0c3fc50f9eedad7ad69936f9d5f5def5900bee5f98be4f0530486e9dd91ecbb0794b424dbeb91182a1c16480e934e623, 0x178a8d4e4b2bd0e54427fc17341f44fe71468d07c33c41e104a256a186d4321b6e6c8eadfb7b394528221bb331ee0d0d),
    (0x172a91520e7f421569ac3a3d2b544d2d6b95b8d4c5f34d07c8c579519b689c19af7172c18a4ab122fe5a9d4455959927, 0x0cd59d2a3f1fc18de3ea73dc2af736d723c28397c9b6646c846a65157f038a5a8f74b0aa2a3f87fa76128c949b195b4e),
    (0x121f20e4845fee99cb13a8c33e4674294d2eec43bf7870381ff9d10cd27c2001c369413bfb0d11cf12ab03c8ac3ee7e0, 0x1938f217f7e79d9e3cfd079753d9e32c9dcbba3610e7d8f697eda50736235762a7136dcc1904cdd8e2ef708ae530fc22),
    (0x039c27044916411f4ddb3a1a0e1687842cc7bf7c68b9de34eb08bedb71bf277269f1862c65e636153f2ef1308e157243, 0x13e72158c15428616264decb1e815be6bb1ed576901768fea30cf8cdb7b73a37e9d18e19107f06582250293a6976157f),
    (0x039c27044916411f4ddb3a1a0e1687842cc7bf7c68b9de34eb08bedb71bf277269f1862c65e636153f2ef1308e157243, 0x13e72158c15428616264decb1e815be6bb1ed576901768fea30cf8cdb7b73a37e9d18e19107f06582250293a6976157f),
    (0x18c7df10aef3f0728d1411ed920060c7b0e2b20814d60ebf6761cdf2a2fdb581f5d73feebcb28571aebccb78070957bf, 0x02d87a4944f7716f65c916298dcfc9c309da0046fd4da4116e61847bb708a8d9035a5dc08b614c17ac0441dfe80f9e9c)
  ]

p_commitment_val_pairs :: [(Integer, Integer)]
p_commitment_val_pairs =
  [
    (0x03b79d7fc3386ab2e6e2079ce1b3ecb13425c1dc545b73a0b15f4b357c18c05c62312c3294c0e97ad6c7efac560b2880, 0x12d28c44b0f7b2cc1d549b35c7ff122fe72873da628019239d9acdeceafae30eb4d242333318eb31d3f1769d2bbecb88),
    (0x10e276e8981050049adf7ffceef9a99237c45e864a6ec51323753ba4baeb61f191a402fd1fecc5dd7e5f6a132c705276, 0x14143ed2ac9330377741f1f1238af6c03f7559e4047a597d6cc099f6eb126a0723743518eb73bf3a005baeeedc6d161a),
    (0x16f2063dcff8148b482875771844f19bdcc204dbb4e2a6043505bdc0a7f144121816b22cbfb0eb1e414831818a8698f7, 0x08a87237a8ca21706f2a24238f4de7858c74b5d029b45dfe768f3ca2e398851efc974dde5d8a604a8e0a1a629039f542),
    (0x0ca0f8315bc3a6f01489af0bed7ba6afb332009de572b88dcbf7fe8d136833d93d12ab1e451a3575f0782883faaf0afe, 0x0c491b263fc80b8a1d86ec982d5f1424e7b7fcdde74b2be936be72d14899e0ba2569cdb569546ae45fa78d7c008b0228)
  ]

commitments :: [(Integer, Integer)] -> [BuiltinBLS12_381_G1_Element]
commitments = fmap (constructG1Point . bimap mkFp mkFp)

p_commitments :: [BuiltinBLS12_381_G1_Element]
p_commitments = commitments p_commitment_val_pairs

f_commitments :: [BuiltinBLS12_381_G1_Element]
f_commitments = commitments f_commitments_val_pairs

p1_commitment :: BuiltinBLS12_381_G1_Element
p1_commitment = p_commitments !! 0
p2_commitment :: BuiltinBLS12_381_G1_Element
p2_commitment = p_commitments !! 1
p3_commitment :: BuiltinBLS12_381_G1_Element
p3_commitment = p_commitments !! 2
p4_commitment :: BuiltinBLS12_381_G1_Element
p4_commitment = p_commitments !! 3


f1_commitment :: BuiltinBLS12_381_G1_Element
f1_commitment = f_commitments !! 0
f2_commitment :: BuiltinBLS12_381_G1_Element
f2_commitment = f_commitments !! 1
f3_commitment :: BuiltinBLS12_381_G1_Element
f3_commitment = f_commitments !! 2
f4_commitment :: BuiltinBLS12_381_G1_Element
f4_commitment = f_commitments !! 3
f5_commitment :: BuiltinBLS12_381_G1_Element
f5_commitment = f_commitments !! 4
f6_commitment :: BuiltinBLS12_381_G1_Element
f6_commitment = f_commitments !! 5
f7_commitment :: BuiltinBLS12_381_G1_Element
f7_commitment = f_commitments !! 6
f8_commitment :: BuiltinBLS12_381_G1_Element
f8_commitment = f_commitments !! 7
f9_commitment :: BuiltinBLS12_381_G1_Element
f9_commitment = f_commitments !! 8


s_g2_val_bbs :: BuiltinByteString
s_g2_val_bbs =
  stringToBuiltinByteStringHex
    "92ff41673671512047d062b24263fa56c318bdd836231669239cc76fe2193a741faf178b2147b647209868e2edd0ae5b16b838a496b7d8bf4ba10f02703c88923fbc40abf2c3c72e48f6b65bb4a0c05d2ab498925722ce13c85512efa42e8240"

s_g2_val :: BuiltinBLS12_381_G2_Element
s_g2_val = bls12_381_G2_uncompress s_g2_val_bbs

omega_val :: Scalar
omega_val =
  mkScalar
    (0x47c8b5817018af4fc70d0874b0691d4e46b3105f04db5844cd3979122d3ea03a `modulo` bls12_381_field_prime)

omegaInv_val :: Scalar
omegaInv_val =
  mkScalar
    (0x5f3ed180c5146cab91c8e2247cfa536dbebeb8129b0029a5df9739f78f26d89e `modulo` bls12_381_field_prime)

barycentricWeight_val :: Scalar
barycentricWeight_val =
  mkScalar
    (0x73eda3b3bc62e3fb474f966e3ae197b844fd796512de43ff0d2007ff00000801 `modulo` bls12_381_field_prime)

transcriptRepr :: Scalar
transcriptRepr =
  mkScalar
    (0x55f1bd796af5b56bc6dbed3a8cb01cbf497741e419e556eb1a9fc39bfcf5e7dc `modulo` bls12_381_field_prime)

blinding_factors :: Integer
blinding_factors = 5
