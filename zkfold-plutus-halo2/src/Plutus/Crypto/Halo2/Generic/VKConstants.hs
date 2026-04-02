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
    (0x0810aee1d3f84122b33ff2a2bede5653d7b35bdca1c638b78c8a55c1401370fad67ded95566480496071dd9db4537758, 0x0d9adcf8da2566dd83c55e640a415036f019eec44144ab9c5a0f5a8d83c7860bf536a9f499666c62467b3dee9b3723e4),
    (0x0dbd2072eca3629fc5ddd55f773a66aeb3e1ff591afc8ffc221ff2278ce7ed78d8d8afeb3f57eb7b482880309491d265, 0x0eaa394953b9ec3158995fe92a119e169b4b008c3c871e42f41709503746d146a8b387c577e18a6fc2652573656bafb6),
    (0x1647f3337511c8248813a8913e30fb4789a1e250c282fc92d473c134ac3d74fbc465d8b3914f7ffdfa30faca4e86bf27, 0x15879da1557e56a123e21fddbfc97bffbcc977ff66a632b82a6254d04adfc638e723c3d6c5aec2f1a3553aad2ac63887),
    (0x18b7d70092dca2df49ec242f8c63fefcda7ab023610f36376d9964e56cb3c3617d54b8ab36767a8d05eafff0f9d2647b, 0x08e4dd22533b55c2a2c015b78da26e7e710f53d12b6239831f3ebbca9885e1cce3aa3f4277b941f587cf54bcf00de5de),
    (0x0323a74a822a64fbff7b8e2f83ef6bc6916bc6179af5af748bbe227d33369db65eb245c651920906a46892473902202d, 0x12564aafff07e033826685f4b099ca0d8d1a43d82cdd556baff22272f6e472498a4278963be19d6ac8584adf070c77fb),
    (0x02764b4b270b80f4e83492838b4e257d7b2a83c4ae9328b0f398ce587f55dbe6c5f0488ae4acdb95fb6330a52eb13e55, 0x0b779440bcd3f664ca7684f8bde17d52924c9ee06a5d3ea361ffa80e6f5982806c7a6a6ccc746ea8bb2530087a1d2d5a),
    (0x148a69bddeade4e5c74ef3a8d87ad89083e5ac53e285e79943ec4cdfe29181251c7d49222227b8d908889fcbeececa29, 0x061dd913d4262b97adf0b78d8d3b63f472ed5965b09862f4396c5ba4da82b4d57651c95784f4f612964295a5a5aaa425),
    (0x148a69bddeade4e5c74ef3a8d87ad89083e5ac53e285e79943ec4cdfe29181251c7d49222227b8d908889fcbeececa29, 0x061dd913d4262b97adf0b78d8d3b63f472ed5965b09862f4396c5ba4da82b4d57651c95784f4f612964295a5a5aaa425),
    (0x10e6fc18162e6f0931b62539d95470c1aa21ced08807edfa59522b52cafc61a28fe7d4bc5569f87bc5c3fccb868b8924, 0x050a03b6113eb29d80f8fab09c154655056c4b9311f949363033250467330aabaf6023ce9cb64256631a765c3719444d)
  ]

p_commitment_val_pairs :: [(Integer, Integer)]
p_commitment_val_pairs =
  [
    (0x13f47ac9ba4c58a8f00d58bf35c6fbe2e0abc76f1683c52e43cdb169903eb0eb73f27f61e0bd1445902855222360ec5a, 0x15fa00bad67ad132ff9d1f0464d51aa720a0dfade7e2ee490055bd6c8d9761a430ec488ab223be816e074768c9f1a6cf),
    (0x15c2b1ab683cbc585da10c6071a1f10944fe4329a87f029102647e8f3bd75870609854d7f79a8baea9f7da1c0bdf431c, 0x046cb79363b243b63f0c406ba5ad68f3e1c1545a4f99b5ecd0d48e3a82910baf29ac0bfd4911c9f34912cbb3e5d749ab),
    (0x12dcd01eab89b63bc8666501d266d38b9c3c56b61ec911792302a084c0acd3babbcca88d734fedc2d797ab79c7520175, 0x0f0c84cf84bba20c1ad03b3a9e74e36463e794d496973995ec4163f6aeda693390da294e946a652be451b2d67d2d046e),
    (0x0e9843457fedd3415b8d0052c5954a9c3d1c163481bf2621588535d9f52b3a9c8e561b452e8f9c3aec6a233e76f3e737, 0x094479484236c0fbe3a5c386f2107b3785087c9b5900ede2a887e9b34599123eb6932a7a8f253fbd598ede85bfedb50c)
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
    (0x520e587a724a6955df625e80d0adef90ad8e16e84419c750194e8c62ecb38d9d `modulo` bls12_381_field_prime)

omegaInv_val :: Scalar
omegaInv_val =
  mkScalar
    (0x059d12338029659fa0e839716ec64c880d93401ca14e79b499196be30704893a `modulo` bls12_381_field_prime)

barycentricWeight_val :: Scalar
barycentricWeight_val =
  mkScalar
    (0x73ed98d574b318148390d1a0cea0d6d118bcf98b4b7dfbff34801fff00002001 `modulo` bls12_381_field_prime)

transcriptRepr :: Scalar
transcriptRepr =
  mkScalar
    (0x14d51aa39046f80c7af9b2af29a9b9336487b471f1ce66f5a4eb3fc4d452e54d `modulo` bls12_381_field_prime)

blinding_factors :: Integer
blinding_factors = 5
