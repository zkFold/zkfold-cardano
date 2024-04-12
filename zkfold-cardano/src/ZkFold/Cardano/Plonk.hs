{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module ZkFold.Cardano.Plonk where

import qualified Data.Vector                              as Vector
import           GHC.ByteOrder                            (ByteOrder (..))
import           GHC.Natural                              (naturalToInteger)
import           PlutusTx.Builtins                        (BuiltinByteString, blake2b_256, bls12_381_G1_uncompress, bls12_381_G2_uncompress, bls12_381_finalVerify, bls12_381_millerLoop, byteStringToInteger, consByteString, emptyByteString, integerToByteString)
import           PlutusTx.List                            (and, foldr, head, map, zipWith)
import           PlutusTx.Prelude                         (Bool (..), divide, enumFromTo, even, otherwise, sum, takeByteString, ($), (&&), (.), (<), (<>), (>), (||))
import qualified PlutusTx.Prelude                         as Plutus
import           Prelude                                  (Integral (..), Show (..), fromInteger, undefined, (++))

import qualified ZkFold.Base.Algebra.Basic.Class          as ZkFold
import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number         (value)
import           ZkFold.Base.Protocol.ARK.Plonk           hiding (F)
import           ZkFold.Base.Protocol.NonInteractiveProof (FromTranscript (..), NonInteractiveProof (..), ToTranscript (..))
import           ZkFold.Cardano.Plonk.Inputs              (InputBytes (..), ProofBytes (..), SetupBytes (..))
import           ZkFold.Cardano.Plonk.Internal            (F (..), challenge, convertF, convertG1, convertG2, getXi, mul, powMod)

type Plonk32 = Plonk 32 BuiltinByteString

data PlonkPlutus

mkSetup :: Setup Plonk32 -> Setup PlonkPlutus
mkSetup (PlonkSetupParams {..}, _, _, PlonkCircuitCommitments {..}, PlonkInput input, _) = SetupBytes
  { n     = naturalToInteger $ value @32
  , g0'   = Plutus.head . Vector.toList . Vector.map convertG1 $ gs
  , h0'   = convertG2 h0
  , h1'   = convertG2 h1
  , omega = F $ convertF omega
  , k1    = F $ convertF k1
  , k2    = F $ convertF k2
  , cmQl' = convertG1 cmQl
  , cmQr' = convertG1 cmQr
  , cmQo' = convertG1 cmQo
  , cmQm' = convertG1 cmQm
  , cmQc' = convertG1 cmQc
  , cmS1' = convertG1 cmS1
  , cmS2' = convertG1 cmS2
  , cmS3' = convertG1 cmS3
  , gens  = map (powMod (F $ convertF omega)) (enumFromTo 1 $ toInteger $ Vector.length input)
  }

mkInput :: Input Plonk32 -> Input PlonkPlutus
mkInput (PlonkInput input) = InputBytes . Vector.toList . Vector.map (F . convertF) $ input

mkProof ::  Setup PlonkPlutus -> Proof Plonk32 -> Proof PlonkPlutus
mkProof SetupBytes{n, gens} (PlonkProof cmA cmB cmC cmZ cmT1 cmT2 cmT3 proof1 proof2 a_xi b_xi c_xi s1_xi s2_xi z_xi) = ProofBytes
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
  , lagsInv = let xi = getXi (convertG1 cmA) (convertG1 cmB) (convertG1 cmC) (convertG1 cmZ) (convertG1 cmT1) (convertG1 cmT2) (convertG1 cmT3)
              in  map ((/) one . (\x -> F n * (xi - x))) gens
  }

instance NonInteractiveProof PlonkPlutus where
    type Transcript PlonkPlutus   = BuiltinByteString
    type Setup PlonkPlutus        = SetupBytes
    type Witness PlonkPlutus      = ()
    type Input PlonkPlutus        = InputBytes
    type Proof PlonkPlutus        = ProofBytes

    setup :: PlonkPlutus -> Setup PlonkPlutus
    setup = undefined

    prove :: Setup PlonkPlutus -> Witness PlonkPlutus -> (Input PlonkPlutus, Proof PlonkPlutus)
    prove = undefined

    -- TODO: Validate arguments
    {-# INLINABLE verify #-}
    verify :: Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> Bool
    verify SetupBytes{..} InputBytes{..} ProofBytes{..} = bls12_381_finalVerify p1 p2
        where
            g0    = bls12_381_G1_uncompress g0'
            h0    = bls12_381_G2_uncompress h0'
            h1    = bls12_381_G2_uncompress h1'
            cmQl  = bls12_381_G1_uncompress cmQl'
            cmQr  = bls12_381_G1_uncompress cmQr'
            cmQo  = bls12_381_G1_uncompress cmQo'
            cmQm  = bls12_381_G1_uncompress cmQm'
            cmQc  = bls12_381_G1_uncompress cmQc'
            cmS1  = bls12_381_G1_uncompress cmS1'
            cmS2  = bls12_381_G1_uncompress cmS2'
            cmS3  = bls12_381_G1_uncompress cmS3'

            cmA    = bls12_381_G1_uncompress cmA'
            cmB    = bls12_381_G1_uncompress cmB'
            cmC    = bls12_381_G1_uncompress cmC'
            cmZ    = bls12_381_G1_uncompress cmZ'
            cmT1   = bls12_381_G1_uncompress cmT1'
            cmT2   = bls12_381_G1_uncompress cmT2'
            cmT3   = bls12_381_G1_uncompress cmT3'
            proof1 = bls12_381_G1_uncompress proof1'
            proof2 = bls12_381_G1_uncompress proof2'
            a_xi   = F a_xi'
            b_xi   = F b_xi'
            c_xi   = F c_xi'
            s1_xi  = F s1_xi'
            s2_xi  = F s2_xi'
            z_xi   = F z_xi'

            (w1 : wxs) = pubInput

            t0 = consByteString 0 $ cmA' <> cmB' <> cmC'
            beta = F . byteStringToInteger BigEndian . takeByteString 31 . blake2b_256 $ t0

            t1 = consByteString 0 t0
            gamma = F . byteStringToInteger BigEndian . takeByteString 31 . blake2b_256 $ t1

            t2 = consByteString 0 $ t1 <> cmZ'
            alpha = F . byteStringToInteger BigEndian . takeByteString 31 . blake2b_256 $ t2

            t3 = consByteString 0 $ t2 <> cmT1' <> cmT2' <> cmT3'
            xi = F . byteStringToInteger BigEndian . takeByteString 31 . blake2b_256 $ t3

            t4 = consByteString 0 $ t3
                <> integerToByteString BigEndian 32 a_xi'
                <> integerToByteString BigEndian 32 b_xi'
                <> integerToByteString BigEndian 32 c_xi'
                <> integerToByteString BigEndian 32 s1_xi'
                <> integerToByteString BigEndian 32 s2_xi'
                <> integerToByteString BigEndian 32 z_xi'
            v = F . byteStringToInteger BigEndian . takeByteString 31 . blake2b_256 $ t4

            u = F . byteStringToInteger BigEndian . takeByteString 31 . blake2b_256 $ t4 <> proof1' <> proof2'

            xi_n = xi `powMod` n
            xi_m_one = xi_n - one

            (lagrange1_xi : lagranges_xi) = zipWith (\x y -> x * xi_m_one * y) gens lagsInv

            pubPoly_xi = w1 * lagrange1_xi + ZkFold.sum (zipWith (*) wxs lagranges_xi)

            alphaSquare = alpha * alpha
            alphaEvalZOmega = alpha * z_xi
            betaZeta = beta * xi

            a_xi_gamma = a_xi + gamma
            b_xi_gamma = b_xi + gamma
            c_xi_gamma = c_xi + gamma

            beta_s1_xi = beta * s1_xi
            beta_s2_xi = beta * s2_xi

            gamma_beta_a_s1 = a_xi_gamma + beta_s1_xi
            gamma_beta_b_s2 = b_xi_gamma + beta_s2_xi

            r0 =
                  pubPoly_xi
                - alphaSquare * lagrange1_xi
                - alphaEvalZOmega
                    * gamma_beta_a_s1
                    * gamma_beta_b_s2
                    * c_xi_gamma

            d  =
                  mul (a_xi * b_xi) cmQm
                + mul a_xi cmQl
                + mul b_xi cmQr
                + mul c_xi cmQo
                + cmQc
                + mul (
                          alpha
                        * (a_xi_gamma + betaZeta)
                        * (b_xi_gamma + betaZeta * k1)
                        * (c_xi_gamma + betaZeta * k2)
                    +     alphaSquare * lagrange1_xi
                    +     u
                    ) cmZ
                - mul (
                      alphaEvalZOmega
                    * beta
                    * gamma_beta_a_s1
                    * gamma_beta_b_s2
                    ) cmS3
                - mul xi_m_one (cmT1 + xi_n `mul` cmT2 + (xi_n * xi_n) `mul` cmT3)

            f  = d
                + v `mul` (cmA
                + v `mul` (cmB
                + v `mul` (cmC
                + v `mul` (cmS1
                + v `mul` cmS2))))

            e  = (
                - r0
                + v * (a_xi
                + v * (b_xi
                + v * (c_xi
                + v * (s1_xi
                + v * s2_xi))))
                + u * z_xi
                ) `mul` g0

            p1 = bls12_381_millerLoop (xi `mul` proof1 + (u * xi * omega) `mul` proof2 + f - e) h0
            p2 = bls12_381_millerLoop (proof1 + u `mul` proof2) h1
