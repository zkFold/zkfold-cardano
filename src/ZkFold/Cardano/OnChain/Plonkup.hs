{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans                 #-}

module ZkFold.Cardano.OnChain.Plonkup where

import           Data.Functor.Rep                            (Rep, Representable)
import           GHC.Base                                    (Ord)
import           GHC.Generics                                (Par1, U1)
import           PlutusTx                                    (UnsafeFromData (..))
import           PlutusTx.Builtins
import           PlutusTx.Prelude                            (Bool (..), BuiltinUnit, check, ($), (&&), (.), (<>), (==))
import           Prelude                                     (undefined)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Protocol.NonInteractiveProof
import           ZkFold.Base.Protocol.Plonkup                (Plonkup)
import           ZkFold.Base.Protocol.Plonkup.Verifier.Setup (PlonkupVerifierSetup (..))
import           ZkFold.Cardano.OffChain.Plonkup             (PlonkupN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.OffChain.Transcript          ()
import           ZkFold.Cardano.OnChain.BLS12_381            (F (..), mul, powTwo)
import           ZkFold.Cardano.OnChain.Plonkup.Data         (InputBytes, ProofBytes (..), SetupBytes (..))

data PlonkupPlutus

instance NonInteractiveProof PlonkupPlutus core where
    type Transcript PlonkupPlutus  = BuiltinByteString
    type SetupProve PlonkupPlutus  = ()
    type SetupVerify PlonkupPlutus = SetupBytes
    type Witness PlonkupPlutus     = ()
    type Input PlonkupPlutus       = InputBytes
    type Proof PlonkupPlutus       = ProofBytes

    setupProve :: PlonkupPlutus -> SetupProve PlonkupPlutus
    setupProve = undefined

    setupVerify :: PlonkupPlutus -> SetupVerify PlonkupPlutus
    setupVerify = undefined

    prove :: SetupProve PlonkupPlutus -> Witness PlonkupPlutus -> (Input PlonkupPlutus, Proof PlonkupPlutus)
    prove = undefined

    {-# INLINABLE verify #-}
    verify :: SetupVerify PlonkupPlutus -> Input PlonkupPlutus -> Proof PlonkupPlutus -> Bool
    verify SetupBytes{..} pi_xi ProofBytes{..} =
        let g0 = bls12_381_G1_uncompress bls12_381_G1_compressed_generator
            h0 = bls12_381_G2_uncompress bls12_381_G2_compressed_generator

            -- uncompress Setup G1 elements
            h1    = bls12_381_G2_uncompress h1_bytes
            cmQm  = bls12_381_G1_uncompress cmQm_bytes
            cmQl  = bls12_381_G1_uncompress cmQl_bytes
            cmQr  = bls12_381_G1_uncompress cmQr_bytes
            cmQo  = bls12_381_G1_uncompress cmQo_bytes
            cmQc  = bls12_381_G1_uncompress cmQc_bytes
            cmQk  = bls12_381_G1_uncompress cmQk_bytes
            cmS1  = bls12_381_G1_uncompress cmS1_bytes
            cmS2  = bls12_381_G1_uncompress cmS2_bytes
            cmS3  = bls12_381_G1_uncompress cmS3_bytes
            cmT1  = bls12_381_G1_uncompress cmT1_bytes

            -- uncompress Proof G1 and wrap Integer to F
            cmA     = bls12_381_G1_uncompress cmA_bytes
            cmB     = bls12_381_G1_uncompress cmB_bytes
            cmC     = bls12_381_G1_uncompress cmC_bytes
            cmF     = bls12_381_G1_uncompress cmF_bytes
            cmH1    = bls12_381_G1_uncompress cmH1_bytes
            cmH2    = bls12_381_G1_uncompress cmH2_bytes
            cmZ1    = bls12_381_G1_uncompress cmZ1_bytes
            cmZ2    = bls12_381_G1_uncompress cmZ2_bytes
            cmQlow  = bls12_381_G1_uncompress cmQlow_bytes
            cmQmid  = bls12_381_G1_uncompress cmQmid_bytes
            cmQhigh = bls12_381_G1_uncompress cmQhigh_bytes
            a_xi    = F a_xi_int
            b_xi    = F b_xi_int
            c_xi    = F c_xi_int
            s1_xi   = F s1_xi_int
            s2_xi   = F s2_xi_int
            f_xi    = F f_xi_int
            t_xi    = F t_xi_int
            t_xi'   = F t_xi'_int
            z1_xi'  = F z1_xi'_int
            z2_xi'  = F z2_xi'_int
            h1_xi'  = F h1_xi'_int
            h2_xi   = F h2_xi_int
            proof1  = bls12_381_G1_uncompress proof1_bytes
            proof2  = bls12_381_G1_uncompress proof2_bytes

            -- create beta, gamma, alpha, xi, v, u from Transcript
            ts1 = cmA_bytes <> cmB_bytes <> cmC_bytes

            ts2 = ts1 <> cmF_bytes <> cmH1_bytes <> cmH2_bytes
            beta    = F . byteStringToInteger LittleEndian . blake2b_224 $ ts2 <> consByteString 1 emptyByteString
            gamma   = F . byteStringToInteger LittleEndian . blake2b_224 $ ts2 <> consByteString 2 emptyByteString
            delta   = F . byteStringToInteger LittleEndian . blake2b_224 $ ts2 <> consByteString 3 emptyByteString
            epsilon = F . byteStringToInteger LittleEndian . blake2b_224 $ ts2 <> consByteString 4 emptyByteString

            ts3 = ts2 <> cmZ1_bytes <> cmZ2_bytes
            alpha = F . byteStringToInteger LittleEndian . blake2b_224 $ ts3
            alpha2 = alpha * alpha
            alpha3 = alpha2 * alpha
            alpha4 = alpha2 * alpha2
            alpha5 = alpha4 * alpha

            ts4 = ts3 <> cmQlow_bytes <> cmQmid_bytes <> cmQhigh_bytes
            xi = F . byteStringToInteger LittleEndian . blake2b_224 $ ts4

            ts5 = ts4
                <> integerToByteString LittleEndian 32 a_xi_int
                <> integerToByteString LittleEndian 32 b_xi_int
                <> integerToByteString LittleEndian 32 c_xi_int
                <> integerToByteString LittleEndian 32 s1_xi_int
                <> integerToByteString LittleEndian 32 s2_xi_int
                <> integerToByteString LittleEndian 32 f_xi_int
                <> integerToByteString LittleEndian 32 t_xi_int
                <> integerToByteString LittleEndian 32 t_xi'_int
                <> integerToByteString LittleEndian 32 z1_xi'_int
                <> integerToByteString LittleEndian 32 z2_xi'_int
                <> integerToByteString LittleEndian 32 h1_xi'_int
                <> integerToByteString LittleEndian 32 h2_xi_int
            v = F . byteStringToInteger LittleEndian . blake2b_224 $ ts5

            ts6 = ts5 <> proof1_bytes <> proof2_bytes
            eta = F . byteStringToInteger LittleEndian . blake2b_224 $ ts6

            -- common varibles for r0, d, f, e

            xi_n = xi `powTwo` pow
            xi_n2 = xi_n * (xi * xi)

            zhX_xi = xi_n - one

            lagrange1_xi = omega * zhX_xi * l1_xi

            cmT_zeta = cmT1

            -- final calculations
            r0 =
                  negate pi_xi * lagrange1_xi
                - alpha * (a_xi + beta * s1_xi + gamma) * (b_xi + beta * s2_xi + gamma) * (c_xi + gamma) * z1_xi'
                - alpha2 * lagrange1_xi
                - alpha4 * z2_xi' * (epsilon * (one + delta) + delta * h2_xi) * (epsilon * (one + delta) + h2_xi + delta * h1_xi')
                - alpha5 * lagrange1_xi

            d  =
                  (a_xi * b_xi) `mul` cmQm + a_xi `mul` cmQl + b_xi `mul` cmQr + c_xi `mul` cmQo + cmQc
                + ((a_xi + beta * xi + gamma) * (b_xi + beta * k1 * xi + gamma) * (c_xi + beta * k2 * xi + gamma) * alpha + lagrange1_xi * alpha2) `mul` cmZ1
                - ((a_xi + beta * s1_xi + gamma) * (b_xi + beta * s2_xi + gamma) * alpha * beta * z1_xi') `mul` cmS3
                + ((a_xi - f_xi) * alpha3) `mul` cmQk
                + ((one + delta) * (epsilon + f_xi) * (epsilon * (one + delta) + t_xi + delta * t_xi') * alpha4 + lagrange1_xi * alpha5) `mul` cmZ2
                - (z2_xi' * (epsilon * (one + delta) + h2_xi + delta * h1_xi') * alpha4) `mul` cmH1
                - zhX_xi `mul` (cmQlow + xi_n2 `mul` cmQmid + (xi_n2 * xi_n2) `mul` cmQhigh)

            f  = d
                + eta `mul` cmZ1
                + v `mul` (cmA + eta `mul` cmT_zeta
                + v `mul` (cmB + eta `mul` cmZ2
                + v `mul` (cmC + eta `mul` cmH1
                + v `mul` (cmS1
                + v `mul` (cmS2
                + v `mul` (cmF
                + v `mul` (cmT_zeta
                + v `mul` cmH2)))))))

            e  = (
                      negate r0 + eta * z1_xi'
                    + v * (a_xi + eta * t_xi'
                    + v * (b_xi + eta * z2_xi'
                    + v * (c_xi + eta * h1_xi'
                    + v * (s1_xi
                    + v * (s2_xi
                    + v * (f_xi
                    + v * (t_xi
                    + v * h2_xi)))))))
                ) `mul` g0

            p1 = bls12_381_millerLoop (xi `mul` proof1 + (eta * xi * omega) `mul` proof2 + f - e) h0
            p2 = bls12_381_millerLoop (proof1 + eta `mul` proof2) h1

        in bls12_381_finalVerify p1 p2 && (l1_xi * F n * (xi - omega) == one)

instance
        ( Representable i
        , KnownNat n
        , Ord (Rep i)
        , SetupVerify (Plonkup U1 i n Par1 c1 c2 ts) ~ PlonkupVerifierSetup U1 i n Par1 c1 c2
        , CoreFunction BLS12_381_G1 core
        ) => CompatibleNonInteractiveProofs (PlonkupN i n) PlonkupPlutus core where
    nipSetupTransform = mkSetup
    nipInputTransform = mkInput
    nipProofTransform = mkProof

untypedPlonkVerifier :: SetupBytes -> BuiltinData -> BuiltinData -> BuiltinUnit
untypedPlonkVerifier computation input' proof' =
    check
    ( verify @PlonkupPlutus @HaskellCore
        computation
        (unsafeFromBuiltinData input')
        (unsafeFromBuiltinData proof')
    )
