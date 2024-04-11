{-# LANGUAGE TypeFamilies #-}

module ZkFold.Cardano.Plonk where

import qualified Data.Vector                              as Vector
import           GHC.ByteOrder                            (ByteOrder (..))
import           GHC.Natural                              (naturalToInteger)
import           PlutusTx.Builtins                        (BuiltinByteString, bls12_381_finalVerify, bls12_381_millerLoop)
import           PlutusTx.List                            (and, foldr, head, map, zipWith)
import           PlutusTx.Prelude                         (Bool (..), divide, enumFromTo, even, otherwise, sum, takeByteString, ($), (&&), (.), (<), (<>), (>), (||))
import qualified PlutusTx.Prelude                         as Plutus
import           Prelude                                  (Show (..), fromInteger, undefined, (++))

import qualified ZkFold.Base.Algebra.Basic.Class          as ZkFold
import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number         (value)
import           ZkFold.Base.Protocol.ARK.Plonk
import           ZkFold.Base.Protocol.NonInteractiveProof (FromTranscript (..), NonInteractiveProof (..), ToTranscript (..))
import           ZkFold.Cardano.Plonk.Inputs
import           ZkFold.Cardano.Plonk.Internal

type Plonk32 = Plonk 32 BuiltinByteString

data PlonkPlutus

mkSetup :: Setup Plonk32 -> Setup PlonkPlutus
mkSetup (PlonkSetupParams {..}, _, _, PlonkCircuitCommitments {..}, _, _) = SetupBytes
  { n'   = naturalToInteger $ value @32
  , gs'  = Vector.toList $ Vector.map convertG1 gs
  , h0'  = convertG2 h0
  , h1'  = convertG2 h1
  , omega' = convertF omega
  , k1'    = convertF k1
  , k2'    = convertF k2
  , cmQl'  = convertG1 cmQl
  , cmQr'  = convertG1 cmQr
  , cmQo'  = convertG1 cmQo
  , cmQm'  = convertG1 cmQm
  , cmQc'  = convertG1 cmQc
  , cmS1'  = convertG1 cmS1
  , cmS2'  = convertG1 cmS2
  , cmS3'  = convertG1 cmS3
  }

mkInput :: Input Plonk32 -> Input PlonkPlutus
mkInput (PlonkInput input) = InputBytes . Vector.toList $ Vector.map convertF input

mkProof :: Proof Plonk32 -> Proof PlonkPlutus
mkProof (PlonkProof cmA cmB cmC cmZ cmT1 cmT2 cmT3 proof1 proof2 a_xi b_xi c_xi s1_xi s2_xi z_xi) = ProofBytes
  { cmA' = convertG1 cmA
  , cmB' = convertG1 cmB
  , cmC' = convertG1 cmC
  , cmZ'  = convertG1 cmZ
  , cmT1' = convertG1 cmT1
  , cmT2' = convertG1 cmT2
  , cmT3' = convertG1 cmT3
  , proof1' = convertG1 proof1
  , proof2' = convertG1 proof2
  , a_xi' = convertF a_xi
  , b_xi' = convertF b_xi
  , c_xi' = convertF c_xi
  , s1_xi' = convertF s1_xi
  , s2_xi' = convertF s2_xi
  , z_xi' = convertF z_xi
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
    verify setup' input' proof' = bls12_381_finalVerify p1 p2
        where
            SetupPlonkPlutus {..} = toSetup setup'
            InputPlonkPlutus {..} = toInput input'
            ProofPlonkPlutus {..} = toProof proof'

            ws@(w1 : wxs) = pubInput

            (beta, ts) = challenge $ Plutus.mempty
                `transcriptG1` cmA
                `transcriptG1` cmB
                `transcriptG1` cmC
            (gamma, t1) = challenge ts

            (alpha, t2) = challenge $ t1
                `transcriptG1` cmZ

            (xi, t3) = challenge $ t2
                `transcriptG1` cmT1
                `transcriptG1` cmT2
                `transcriptG1` cmT3

            (v, t4) = challenge $ t3
                `transcriptF` a_xi
                `transcriptF` b_xi
                `transcriptF` c_xi
                `transcriptF` s1_xi
                `transcriptF` s2_xi
                `transcriptF` z_xi

            (u, _) = challenge $ t4
                `transcriptG1` proof1
                `transcriptG1` proof2

            xi_n = xi `powMod` n
            xi_m_one = xi_n - one

            omegas = Plutus.fmap (powMod omega) (enumFromTo 1 (Plutus.length ws))
            lagsInv = Plutus.fmap ((/) one . (\omg -> F n * (xi - omg))) omegas

            (lagrange1_xi : lagranges_xi) = zipWith (\x y -> x * xi_m_one * y) omegas lagsInv

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
                - mul xi_m_one (cmT1 + xi_n `mul` cmT2 + (xi_n `powMod` 2) `mul` cmT3)

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
                ) `mul` head gs

            p1 = bls12_381_millerLoop (xi `mul` proof1 + (u * xi * omega) `mul` proof2 + f - e) h0
            p2 = bls12_381_millerLoop (proof1 + u `mul` proof2) h1
