{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans                 #-}

module ZkFold.Cardano.OnChain.Plonk where

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
import           ZkFold.Cardano.OffChain.Plonk               (PlonkN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.OffChain.Transcript          ()
import           ZkFold.Cardano.OnChain.BLS12_381            (F (..), mul, powTwo)
import           ZkFold.Cardano.OnChain.Plonk.Data           (InputBytes, ProofBytes (..), SetupBytes (..))

data PlonkPlutus

instance NonInteractiveProof PlonkPlutus core where
    type Transcript PlonkPlutus  = BuiltinByteString
    type SetupProve PlonkPlutus  = ()
    type SetupVerify PlonkPlutus = SetupBytes
    type Witness PlonkPlutus     = ()
    type Input PlonkPlutus       = InputBytes
    type Proof PlonkPlutus       = ProofBytes

    setupProve :: PlonkPlutus -> SetupProve PlonkPlutus
    setupProve = undefined

    setupVerify :: PlonkPlutus -> SetupVerify PlonkPlutus
    setupVerify = undefined

    prove :: SetupProve PlonkPlutus -> Witness PlonkPlutus -> (Input PlonkPlutus, Proof PlonkPlutus)
    prove = undefined

    {-# INLINABLE verify #-}
    verify :: SetupVerify PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> Bool
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
            cmS1  = bls12_381_G1_uncompress cmS1_bytes
            cmS2  = bls12_381_G1_uncompress cmS2_bytes
            cmS3  = bls12_381_G1_uncompress cmS3_bytes

            -- uncompress Proof G1 and wrap Integer to F
            cmA     = bls12_381_G1_uncompress cmA_bytes
            cmB     = bls12_381_G1_uncompress cmB_bytes
            cmC     = bls12_381_G1_uncompress cmC_bytes
            cmZ1    = bls12_381_G1_uncompress cmZ1_bytes
            cmQlow  = bls12_381_G1_uncompress cmQlow_bytes
            cmQmid  = bls12_381_G1_uncompress cmQmid_bytes
            cmQhigh = bls12_381_G1_uncompress cmQhigh_bytes
            a_xi    = F a_xi_int
            b_xi    = F b_xi_int
            c_xi    = F c_xi_int
            s1_xi   = F s1_xi_int
            s2_xi   = F s2_xi_int
            z1_xi'  = F z1_xi'_int
            proof1  = bls12_381_G1_uncompress proof1_bytes
            proof2  = bls12_381_G1_uncompress proof2_bytes

            -- create beta, gamma, alpha, xi, v, u from Transcript
            ts1 = cmA_bytes <> cmB_bytes <> cmC_bytes

            ts2 = ts1
            beta  = F . byteStringToInteger LittleEndian . blake2b_224 $ ts2 <> consByteString 1 emptyByteString
            gamma = F . byteStringToInteger LittleEndian . blake2b_224 $ ts2 <> consByteString 2 emptyByteString

            ts3 = ts2 <> cmZ1_bytes
            alpha  = F . byteStringToInteger LittleEndian . blake2b_224 $ ts3
            alpha2 = alpha * alpha

            ts4 = ts3 <> cmQlow_bytes <> cmQmid_bytes <> cmQhigh_bytes
            xi = F . byteStringToInteger LittleEndian . blake2b_224 $ ts4

            ts5 = ts4
                <> integerToByteString LittleEndian 32 a_xi_int
                <> integerToByteString LittleEndian 32 b_xi_int
                <> integerToByteString LittleEndian 32 c_xi_int
                <> integerToByteString LittleEndian 32 s1_xi_int
                <> integerToByteString LittleEndian 32 s2_xi_int
                <> integerToByteString LittleEndian 32 z1_xi'_int
            v = F . byteStringToInteger LittleEndian . blake2b_224 $ ts5

            ts6 = ts5 <> proof1_bytes <> proof2_bytes
            eta = F . byteStringToInteger LittleEndian . blake2b_224 $ ts6

            -- common varibles for r0, d, f, e

            xi_n = xi `powTwo` pow
            xi_n2 = xi_n * (xi * xi)

            zhX_xi = xi_n - one

            lagrange1_xi = omega * zhX_xi * l1_xi

            -- final calculations
            r0 =
                  negate pi_xi * lagrange1_xi
                - alpha * (a_xi + beta * s1_xi + gamma) * (b_xi + beta * s2_xi + gamma) * (c_xi + gamma) * z1_xi'
                - alpha2 * lagrange1_xi

            d  =
                  mul (a_xi * b_xi) cmQm
                + mul a_xi cmQl
                + mul b_xi cmQr
                + mul c_xi cmQo
                + cmQc
                + mul (
                          alpha
                        * (a_xi + beta * xi + gamma)
                        * (b_xi + beta * k1 * xi + gamma)
                        * (c_xi + beta * k2 * xi + gamma)
                    +     alpha2 * lagrange1_xi
                    +     eta
                    ) cmZ1
                - mul (
                      alpha * z1_xi'
                    * beta
                    * (a_xi + beta * s1_xi + gamma)
                    * (b_xi + beta * s2_xi + gamma)
                    ) cmS3
                - mul zhX_xi (cmQlow + xi_n2 `mul` cmQmid + (xi_n2 * xi_n2) `mul` cmQhigh)

            f  = d
                + v `mul` (cmA
                + v `mul` (cmB
                + v `mul` (cmC
                + v `mul` (cmS1
                + v `mul` cmS2))))

            e  = (
                negate r0
                + v * (a_xi
                + v * (b_xi
                + v * (c_xi
                + v * (s1_xi
                + v * s2_xi))))
                + eta * z1_xi'
                ) `mul` g0

            p1 = bls12_381_millerLoop (xi `mul` proof1 + (eta * xi * omega) `mul` proof2 + f - e) h0
            p2 = bls12_381_millerLoop (proof1 + eta `mul` proof2) h1

        in bls12_381_finalVerify p1 p2 && (l1_xi * F n * (xi - omega) == one)

instance
        ( KnownNat i
        , KnownNat n
        , SetupVerify (Plonkup i n 1 c1 c2 ts) ~ PlonkupVerifierSetup i n 1 c1 c2
        , CoreFunction BLS12_381_G1 core
        ) => CompatibleNonInteractiveProofs (PlonkN i n) PlonkPlutus core where
    nipSetupTransform = mkSetup
    nipInputTransform = mkInput
    nipProofTransform = mkProof

untypedVerifyPlonk :: SetupBytes -> BuiltinData -> BuiltinData -> BuiltinUnit
untypedVerifyPlonk computation input' proof' =
    check
    ( verify @PlonkPlutus @HaskellCore
        computation
        (unsafeFromBuiltinData input')
        (unsafeFromBuiltinData proof')
    )
