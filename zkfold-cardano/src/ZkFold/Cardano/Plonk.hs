{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module ZkFold.Cardano.Plonk where

import qualified Data.Vector                              as Vector
import           GHC.Natural                              (naturalToInteger)
import           PlutusTx.Builtins
import           PlutusTx.Prelude                         (Bool (..), ($))
import qualified PlutusTx.Prelude                         as Plutus
import           Prelude                                  (fromInteger, undefined, (.))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number         (value)
import           ZkFold.Base.Protocol.ARK.Plonk
import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk.Inputs
import           ZkFold.Cardano.Plonk.Internal

type PlonkBBS = Plonk 32 BuiltinByteString

data PlonkPlutus

mkSetup :: Setup PlonkBBS -> Setup PlonkPlutus
mkSetup (PlonkSetupParams {..}, _, _, PlonkCircuitCommitments {..}, _, _) = SetupPlonkPlutus
  { n   = naturalToInteger $ value @32
  , g0  = convertG1 $ Vector.head gs
  , h0  = convertG2 h0
  , h1  = convertG2 h1
  , omega = convertF omega
  , k1    = convertF k1
  , k2    = convertF k2
  , cmQl  = convertG1 cmQl
  , cmQr  = convertG1 cmQr
  , cmQo  = convertG1 cmQo
  , cmQm  = convertG1 cmQm
  , cmQc  = convertG1 cmQc
  , cmS1  = convertG1 cmS1
  , cmS2  = convertG1 cmS2
  , cmS3  = convertG1 cmS3
  }

mkInput :: Input PlonkBBS -> Input PlonkPlutus
mkInput (PlonkInput input) = InputPlonkPlutus . convertF . Vector.head $ input

mkProof :: Proof PlonkBBS -> Proof PlonkPlutus
mkProof (PlonkProof cmA cmB cmC cmZ cmT1 cmT2 cmT3 proof1 proof2 a_xi b_xi c_xi s1_xi s2_xi z_xi) = ProofPlonkPlutus
  { cmA = convertG1 cmA
  , cmB = convertG1 cmB
  , cmC = convertG1 cmC
  , cmZ  = convertG1 cmZ
  , cmT1 = convertG1 cmT1
  , cmT2 = convertG1 cmT2
  , cmT3 = convertG1 cmT3
  , proof1 = convertG1 proof1
  , proof2 = convertG1 proof2
  , a_xi = convertF a_xi
  , b_xi = convertF b_xi
  , c_xi = convertF c_xi
  , s1_xi = convertF s1_xi
  , s2_xi = convertF s2_xi
  , z_xi = convertF z_xi
  }

instance NonInteractiveProof PlonkPlutus where
    type Transcript PlonkPlutus   = BuiltinByteString
    type Setup PlonkPlutus        = SetupPlonkPlutus
    type Witness PlonkPlutus      = ()
    type Input PlonkPlutus        = InputPlonkPlutus
    type Proof PlonkPlutus        = ProofPlonkPlutus

    setup :: PlonkPlutus -> Setup PlonkPlutus
    setup = undefined

    prove :: Setup PlonkPlutus -> Witness PlonkPlutus -> (Input PlonkPlutus, Proof PlonkPlutus)
    prove = undefined

    -- TODO: Validate arguments
    {-# INLINABLE verify #-}
    verify :: Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> Bool
    verify (SetupPlonkPlutus {..}) (InputPlonkPlutus {..}) (ProofPlonkPlutus {..}) = bls12_381_finalVerify p1 p2
        where
            (beta, ts) = challenge $ emptyByteString
                `transcriptG1` cmA
                `transcriptG1` cmB
                `transcriptG1` cmC
            (gamma, ts') = challenge ts

            (alpha, ts'') = challenge $ ts' `transcriptG1` cmZ

            (xi, ts''') = challenge $ ts''
                `transcriptG1` cmT1
                `transcriptG1` cmT2
                `transcriptG1` cmT3

            (v, ts'''') = challenge $ ts'''
                `transcriptF` a_xi
                `transcriptF` b_xi
                `transcriptF` c_xi
                `transcriptF` s1_xi
                `transcriptF` s2_xi
                `transcriptF` z_xi

            (u, _) = challenge $ ts''''
                `transcriptG1` proof1
                `transcriptG1` proof2

            zH_xi        = xi `powMod` n - one
            lagrange1_xi = omega * zH_xi / (fromInteger n * (xi - omega))
            omega2       = omega * omega
            lagrange2_xi = omega2 * zH_xi / (F n * (xi - omega2))
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
                - mul zH_xi (cmT1 + (xi `powMod` n) `mul` cmT2 + (xi `powMod` (2 Plutus.* n)) `mul` cmT3)
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

            p1 = bls12_381_millerLoop (xi `mul` proof1 + (u * xi * omega) `mul` proof2 + f - e) h0
            p2 = bls12_381_millerLoop (proof1 + u `mul` proof2) h1
