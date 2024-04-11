{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}

module ZkFold.Cardano.Plonk where

import qualified Data.Vector                              as Vector
import           GHC.ByteOrder                            (ByteOrder (..))
import           GHC.Natural                              (naturalToInteger)
import           PlutusTx.Builtins                        (BuiltinByteString, bls12_381_finalVerify, bls12_381_millerLoop, emptyByteString)
import           PlutusTx.List                            (and, foldr, head, map, zipWith)
import           PlutusTx.Prelude                         (Bool (..), divide, enumFromTo, even, otherwise, sum, takeByteString, ($), (&&), (.), (<), (<>), (>), (||))
import qualified PlutusTx.Prelude                         as Plutus
import           Prelude                                  (Show (..), fromInteger, undefined, (++), Integral (..))

import qualified ZkFold.Base.Algebra.Basic.Class          as ZkFold
import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number         (value)
import           ZkFold.Base.Protocol.ARK.Plonk hiding (F)
import           ZkFold.Base.Protocol.NonInteractiveProof (FromTranscript (..), NonInteractiveProof (..), ToTranscript (..))
import           ZkFold.Cardano.Plonk.Inputs
import           ZkFold.Cardano.Plonk.Internal
import           Debug.Trace(trace)

type Plonk32 = Plonk 32 BuiltinByteString

data PlonkPlutus

getXi :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString ->
         BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> F
getXi cmA' cmB' cmC' cmZ' cmT1' cmT2' cmT3' = xi
  where (beta, ts) = challenge $ cmA'
            `transcriptG1Byte` cmB'
            `transcriptG1Byte` cmC'
        (gamma, t1) = challenge ts

        (alpha, t2) = challenge $ t1
            `transcriptG1Byte` cmZ'

        (xi, t3) = challenge $ t2
            `transcriptG1Byte` cmT1'
            `transcriptG1Byte` cmT2'
            `transcriptG1Byte` cmT3'

mkSetup :: Setup Plonk32 -> Setup PlonkPlutus
mkSetup (PlonkSetupParams {..}, _, _, PlonkCircuitCommitments {..}, PlonkInput input, _) = SetupBytes
  { n'   = naturalToInteger $ value @32
  , g0'  = Plutus.head . Vector.toList . Vector.map convertG1 $ gs
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
  , gens'  = map (powMod (F $ convertF omega)) (enumFromTo 1 $ toInteger $ Vector.length input)
  }

mkInput :: Input Plonk32 -> Input PlonkPlutus
mkInput (PlonkInput input) = InputBytes . Vector.toList . Vector.map convertF $ input

mkProof ::  Setup PlonkPlutus -> Proof Plonk32 -> Proof PlonkPlutus
mkProof SetupBytes{n', gens'} (PlonkProof cmA cmB cmC cmZ cmT1 cmT2 cmT3 proof1 proof2 a_xi b_xi c_xi s1_xi s2_xi z_xi) = ProofBytes
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
  , lagsInv' = let xi = getXi (convertG1 cmA) (convertG1 cmB) (convertG1 cmC) (convertG1 cmZ) (convertG1 cmT1) (convertG1 cmT2) (convertG1 cmT3)
               in  map ((/) one . (\x -> F n' * (xi - x))) gens'
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
    verify setup'@SetupBytes{..} input'@InputBytes{..} proof'@ProofBytes{..} = bls12_381_finalVerify p1 p2
        where
            SetupPlonkPlutus {..} = toSetup setup'
            InputPlonkPlutus {..} = toInput input'
            ProofPlonkPlutus {..} = toProof proof'
            
            (w1 : wxs) = pubInput

            (beta, ts) = challenge $ cmA'
                `transcriptG1Byte` cmB'
                `transcriptG1Byte` cmC'
            (gamma, t1) = challenge ts

            (alpha, t2) = challenge $ t1
                `transcriptG1Byte` cmZ'

            (xi, t3) = challenge $ t2
                `transcriptG1Byte` cmT1'
                `transcriptG1Byte` cmT2'
                `transcriptG1Byte` cmT3'

            (v, t4) = challenge $ t3
                `transcriptFByte` a_xi'
                `transcriptFByte` b_xi'
                `transcriptFByte` c_xi'
                `transcriptFByte` s1_xi'
                `transcriptFByte` s2_xi'
                `transcriptFByte` z_xi'

            u = fromTranscript $ t4
                `transcriptG1Byte` proof1'
                `transcriptG1Byte` proof2'

            xi_n = xi `powMod` n
            xi_m_one = xi_n - one

            (lagrange1_xi : lagranges_xi) = zipWith (\x y -> x * xi_m_one * y) gens' lagsInv'

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
