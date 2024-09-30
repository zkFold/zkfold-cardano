{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans                 #-}

module ZkFold.Cardano.OnChain.Plonk where

import           GHC.ByteOrder                               (ByteOrder (..))
import           PlutusTx                                    (UnsafeFromData (..))
import           PlutusTx.Builtins
import           PlutusTx.Prelude                            (Bool (..), BuiltinUnit, check, ($), (&&), (.), (<>), (==))
import           Prelude                                     (undefined)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Protocol.NonInteractiveProof
import           ZkFold.Base.Protocol.Plonkup                (Plonkup)
import           ZkFold.Base.Protocol.Plonkup.Verifier.Setup (PlonkupVerifierSetup (..))

import           ZkFold.Cardano.OnChain.BLS12_381            (F (..), powTwo, mul)
import           ZkFold.Cardano.OnChain.Plonk.Data           (InputBytes (..), ProofBytes (..), SetupBytes (..))
import           ZkFold.Cardano.OffChain.Plonk               (PlonkN, mkInput, mkProof, mkSetup)

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
    verify SetupBytes{..} InputBytes{..} ProofBytes{..} =
        let g0 = bls12_381_G1_uncompress bls12_381_G1_compressed_generator
            h0 = bls12_381_G2_uncompress bls12_381_G2_compressed_generator

            -- uncompress Setup G1 elements
            h1    = bls12_381_G2_uncompress x2'
            cmQm  = bls12_381_G1_uncompress cmQm'
            cmQl  = bls12_381_G1_uncompress cmQl'
            cmQr  = bls12_381_G1_uncompress cmQr'
            cmQo  = bls12_381_G1_uncompress cmQo'
            cmQc  = bls12_381_G1_uncompress cmQc'
            cmS1  = bls12_381_G1_uncompress cmS1'
            cmS2  = bls12_381_G1_uncompress cmS2'
            cmS3  = bls12_381_G1_uncompress cmS3'

            -- uncompress Proof G1 and wrap Integer to F
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

            -- create beta, gamma, alpha, xi, v, u from Transcript
            ts1 = cmA' <> cmB' <> cmC'

            ts2 = ts1
            beta  = F . byteStringToInteger LittleEndian . blake2b_224 $ ts2 <> consByteString 1 emptyByteString
            gamma = F . byteStringToInteger LittleEndian . blake2b_224 $ ts2 <> consByteString 2 emptyByteString

            ts3 = ts2 <> cmZ'
            alpha = F . byteStringToInteger LittleEndian . blake2b_224 $ ts3

            ts4 = ts3 <> cmT1' <> cmT2' <> cmT3'
            xi = F . byteStringToInteger LittleEndian . blake2b_224 $ ts4

            ts5 = ts4
                <> integerToByteString LittleEndian 32 a_xi'
                <> integerToByteString LittleEndian 32 b_xi'
                <> integerToByteString LittleEndian 32 c_xi'
                <> integerToByteString LittleEndian 32 s1_xi'
                <> integerToByteString LittleEndian 32 s2_xi'
                <> integerToByteString LittleEndian 32 z_xi'
            v = F . byteStringToInteger LittleEndian . blake2b_224 $ ts5

            ts6 = ts5 <> proof1' <> proof2'
            u = F . byteStringToInteger LittleEndian . blake2b_224 $ ts6

            -- common varibles for r0, d, f, e

            xi_n = xi `powTwo` pow
            xi_n2 = xi_n * (xi * xi)
            xi_m_one = xi_n - one

            lagrange1_xi = omega * xi_m_one * l1_xi_mul'

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

            -- final calculations
            r0 =
                  negate pubInput * lagrange1_xi
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
                - mul xi_m_one (cmT1 + xi_n2 `mul` cmT2 + (xi_n2 * xi_n2) `mul` cmT3)

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
                + u * z_xi
                ) `mul` g0

            p1 = bls12_381_millerLoop (xi `mul` proof1 + (u * xi * omega) `mul` proof2 + f - e) h0
            p2 = bls12_381_millerLoop (proof1 + u `mul` proof2) h1

        in bls12_381_finalVerify p1 p2 && (l1_xi_mul' * F n * (xi - omega) == one)

instance
        ( KnownNat i
        , KnownNat n
        , KnownNat (3 * n)
        , KnownNat ((4 * n) + 6)
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
