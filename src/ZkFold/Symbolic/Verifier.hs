{-# LANGUAGE TypeApplications #-}

module ZkFold.Symbolic.Verifier where

import           Data.ByteString                             (empty)
import           Prelude                                     hiding (Num(..), (^), (/), (!!), sum, length, take, drop, replicate)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field             (toZp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk)
import           ZkFold.Base.Protocol.Commitment.KZG
import           ZkFold.Base.Protocol.NonInteractiveProof

data PlonkPlutus

mkSetup :: Setup Plonk -> Setup PlonkPlutus
mkSetup ((_, gs, h0, h1, omega, k1, k2), _, (cmQl, cmQr, cmQo, cmQm, cmQc, cmS1, cmS2, cmS3), _, _) =
    ((order @Plonk, head gs, h0, h1, omega, k1, k2), (cmQl, cmQr, cmQo, cmQm, cmQc, cmS1, cmS2, cmS3))

mkInput :: Input Plonk -> Input PlonkPlutus
mkInput = head

instance NonInteractiveProof PlonkPlutus where
    type Params PlonkPlutus       = ()
    type SetupSecret PlonkPlutus  = ()
    type Setup PlonkPlutus        = ((Integer, G1, G2, G2, F, F, F), (G1, G1, G1, G1, G1, G1, G1, G1))
    type ProverSecret PlonkPlutus = ()
    type Witness PlonkPlutus      = ()
    type Input PlonkPlutus        = F
    type Proof PlonkPlutus        = (G1, G1, G1, G1, G1, G1, G1, G1, G1, F, F, F, F, F, F)

    setup :: Params PlonkPlutus -> SetupSecret PlonkPlutus -> Setup PlonkPlutus
    setup = undefined

    prove :: ProverSecret PlonkPlutus -> Setup PlonkPlutus -> Witness PlonkPlutus -> (Input PlonkPlutus, Proof PlonkPlutus)
    prove = undefined

    -- TODO: Validate arguments
    verify :: Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> Bool
    verify
        ((n, g0, h0, h1, omega, k1, k2), (cmQl, cmQr, cmQo, cmQm, cmQc, cmS1, cmS2, cmS3))
        pubInput
        (cmA, cmB, cmC, cmZ, cmT1, cmT2, cmT3, proof1, proof2, a_xi, b_xi, c_xi, s1_xi, s2_xi, z_xi) = p1 == p2
        where
            (beta, ts) = challenge $ empty
                `transcript` cmA
                `transcript` cmB
                `transcript` cmC :: (F, Transcript)
            (gamma, ts') = challenge ts

            (alpha, ts'') = challenge $ ts' `transcript` cmZ

            (xi, ts''') = challenge $ ts''
                `transcript` cmT1
                `transcript` cmT2
                `transcript` cmT3

            (v, ts'''') = challenge $ ts'''
                `transcript` a_xi
                `transcript` b_xi
                `transcript` c_xi
                `transcript` s1_xi
                `transcript` s2_xi
                `transcript` z_xi

            (u, _) = challenge $ ts''''
                `transcript` proof1
                `transcript` proof2

            zH_xi        = xi^n - one :: F
            lagrange1_xi = omega * zH_xi / (toZp n * (xi - omega))
            omega2       = omega * omega
            lagrange2_xi = omega2 * zH_xi / (toZp n * (xi - omega2))
            pubPoly_xi   = pubInput * lagrange1_xi - lagrange2_xi

            r0 =
                  pubPoly_xi
                - alpha * alpha * lagrange1_xi
                - alpha
                    * (a_xi + beta * s1_xi + gamma)
                    * (b_xi + beta * s2_xi + gamma)
                    * (c_xi + gamma)
                    * z_xi
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
                    +     alpha * alpha * lagrange1_xi
                    +     u
                    ) cmZ
                - mul (
                      alpha
                    * beta
                    * (a_xi + beta * s1_xi + gamma)
                    * (b_xi + beta * s2_xi + gamma)
                    * z_xi
                    ) cmS3
                - mul zH_xi (cmT1 + (xi^n) `mul` cmT2 + (xi^(2*n)) `mul` cmT3)
            f  =
                  d
                + v `mul` cmA
                + (v * v) `mul` cmB
                + (v * v * v) `mul` cmC
                + (v * v * v * v) `mul` cmS1
                + (v * v * v * v * v) `mul` cmS2
            e  = (
                - r0
                + v * a_xi
                + v * v * b_xi
                + v * v * v * c_xi
                + v * v * v * v * s1_xi
                + v * v * v * v * v * s2_xi
                + u * z_xi
                ) `mul` g0

            p1 = pairing (xi `mul` proof1 + (u * xi * omega) `mul` proof2 + f - e) h0
            p2 = pairing (proof1 + u `mul` proof2) h1