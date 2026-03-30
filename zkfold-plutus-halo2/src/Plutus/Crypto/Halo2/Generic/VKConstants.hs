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
    (0x0ce1d87d39930f023970992b349f8057e3b315330e0eec9fb64e7d9057b8f1c490e66a0b0b3c00b6ae32e79d2cb7e028, 0x17656ff18f66517e1cb5917bb9aff70b7eb1d2011a3fb550d86b4c43df055903cce788b9c384ce4986c7634914c0ea83),
    (0x18f37990c9f13bf46956ad7236c2e1a350eb5bf9222400c649357e4a96bc71d30618477fae12413c3e2507b923adf10b, 0x03d120b81b5e6565063c2dd67fa55601aaa595ebb587657a181f214c2ad39df307cca18cb80473a66eb855a86244afab),
    (0x09f7fc48174c7c55151e5bb639b4210dad84dea7d75c0d437ff5140f5c886533b174a8af4165f90a089b3735b78fe9a0, 0x003ef4ea72a5f9771d991e3adc81de34275f1af531a2060ad5588dfe18dfd12cb2e1a4cc6ab88046851135f8281de64b),
    (0x0dfcb53f61fbac0da9b6c71f8371971b6deefb7e5510580cb76d5395bc4908588d0deb02d58c3c073e967a61a458f191, 0x0a7d7849c55c76bd6e7407e664cc045820b8d53e747a0a987312c126fa414dcdf83bce73292c178f944f154ec0e9c2fd),
    (0x154170608aa03ff53e35951ed489697078619c10e62d043b5e882344ab235de2de1173eb4dae651ea0bcf3c17e3cd115, 0x00a3cb75d85d886f452950ddef97dfbb43cda0d5f1733679d84752ce2ab90de85e0a7a5a12d83b77e06339c06ed1762b),
    (0x16a119bc3bef8be1f00d72fbc45c96e9eb11545871be0b927ff2cfbda53a7d29a7d504c9a2e4b283c9560016f918564a, 0x09cf867960d8a5701068b26da59467613745ac2ed9832c697447c29e9f9bbce512a9554961adc829aa20a4fb104a4a6c),
    (0x077b47bf22ff0e48881a81d876490401c0a6ee543372df7210ceb230ea5a4d1d56d7800904f195117a1502b27f6b6290, 0x16c9f1ae07c8bc90afdd710cdd8c62c053e38ee4ade2c9cc65eab1f57ed68894dfd5e9235369810c9ef25d9b96309fd1),
    (0x077b47bf22ff0e48881a81d876490401c0a6ee543372df7210ceb230ea5a4d1d56d7800904f195117a1502b27f6b6290, 0x16c9f1ae07c8bc90afdd710cdd8c62c053e38ee4ade2c9cc65eab1f57ed68894dfd5e9235369810c9ef25d9b96309fd1),
    (0x17adce98ad46926e8bf6aba97a81c406ad7b6420eada53c6daec1de2cd4a87e64409c6d081a3d15751493a887c75d0be, 0x0c15881c7ce718620802c23eaed9164961e031348577613aa5a0ba997a1d767a671c63e87d412b027a44aa57bf47327d)
  ]

p_commitment_val_pairs :: [(Integer, Integer)]
p_commitment_val_pairs =
  [
    (0x045e0519c7613e020103def7e6f748803b3c12b5c00630f4e03307c63aa77f9bf1cec5b0e9425b10041ae9b1f74efa9a, 0x0474b07d8cd3438eac197c44460d039423ece07cb57caa04ccaa9aecb523a1cfffcc682c1a97bd57586eff0921baa7fa),
    (0x06b9bd268c81e4412a2ef55530187d21e21c9557844b7d1e4910a5e9398a8edd8350ec9b9db3cc8625743ec3773f68e0, 0x03ad4797dc533dc8d8704d5582119d411651cdfbc09b57ce76c9eb00a99febfc5ab4499d08d0e5b5d942acc786a2043a),
    (0x118a624ee6cef847dbcaaf28588db1b099ce824420721bd65966e189ebca58b41e59b923a29302b9693c3707ab52b9b0, 0x0317d614a62be9d8ea987990f75a1480d0deea0154923dfc8645422a7fd4d9ec23f5adacc6061be11f88094f3c5c4d7b),
    (0x0c01be9e98bcebbe4b705ff12e72f340081e1f887bd9a45405c63a69c2344d14db07aad22a8aa9fc5619196586e38134, 0x10c82d68ee59ffb12535f7e489fe01bc1293b5875457dcc5219ee39e214168bf6e6ffcd23c67277ad1e3d9699247263d)
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
    (0x1b21c3194f2f05960f0e5a82c620deca9b14e65ceca5e1b7cf51ff7773e6aeaa `modulo` bls12_381_field_prime)

blinding_factors :: Integer
blinding_factors = 5
