{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.OffChain.Plonkup where

import           Data.Foldable                                     (Foldable, toList)
import           PlutusTx.Builtins                                 (BuiltinByteString)
import           PlutusTx.Prelude                                  (($), (.))
import           Prelude                                           (fromIntegral, map)

import           ZkFold.Base.Algebra.Basic.Number                  (KnownNat, value)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381       (BLS12_381_G1_Point, BLS12_381_G2_Point, Fr)
import           ZkFold.Base.Algebra.Polynomials.Univariate        (PolyVec)
import           ZkFold.Base.Protocol.NonInteractiveProof          (NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonkup                      (Plonkup)
import           ZkFold.Base.Protocol.Plonkup.Input                (PlonkupInput (..))
import           ZkFold.Base.Protocol.Plonkup.Proof
import           ZkFold.Base.Protocol.Plonkup.Verifier.Commitments
import           ZkFold.Base.Protocol.Plonkup.Verifier.Setup
import           ZkFold.Cardano.OffChain.BLS12_381                 (convertG1, convertG2, convertZp)
import           ZkFold.Cardano.OnChain.BLS12_381.F                (F (..))
import           ZkFold.Cardano.OnChain.Plonkup.Data               (InputBytes, ProofBytes (..), SetupBytes (..))
import           ZkFold.Prelude                                    (log2ceiling)

--------------- Transform Plonk Base to Plonk BuiltinByteString ----------------

type PlonkupN i n l = Plonkup i n l BLS12_381_G1_Point BLS12_381_G2_Point BuiltinByteString (PolyVec Fr)

mkSetup :: forall i n l . KnownNat n => SetupVerify (PlonkupN i n l) -> SetupBytes
mkSetup PlonkupVerifierSetup {..} =
  let PlonkupCircuitCommitments {..} = commitments
  in SetupBytes
    { n          = fromIntegral (value @n)
    , pow        = log2ceiling (value @n)
    , omega      = F $ convertZp omega
    , k1         = F $ convertZp k1
    , k2         = F $ convertZp k2
    , h1_bytes   = convertG2 h1
    , cmQm_bytes = convertG1 cmQm
    , cmQl_bytes = convertG1 cmQl
    , cmQr_bytes = convertG1 cmQr
    , cmQo_bytes = convertG1 cmQo
    , cmQc_bytes = convertG1 cmQc
    , cmQk_bytes = convertG1 cmQk
    , cmS1_bytes = convertG1 cmS1
    , cmS2_bytes = convertG1 cmS2
    , cmS3_bytes = convertG1 cmS3
    , cmT1_bytes = convertG1 cmT1
    }

mkInput :: Foldable l => Input (PlonkupN i n l) -> InputBytes
mkInput (PlonkupInput input) = map (F . convertZp) $ toList input

mkProof :: Proof (PlonkupN i n l) -> ProofBytes
mkProof PlonkupProof {..} = ProofBytes
  { cmA_bytes     = convertG1 cmA
  , cmB_bytes     = convertG1 cmB
  , cmC_bytes     = convertG1 cmC
  , cmF_bytes     = convertG1 cmF
  , cmH1_bytes    = convertG1 cmH1
  , cmH2_bytes    = convertG1 cmH2
  , cmZ1_bytes    = convertG1 cmZ1
  , cmZ2_bytes    = convertG1 cmZ2
  , cmQlow_bytes  = convertG1 cmQlow
  , cmQmid_bytes  = convertG1 cmQmid
  , cmQhigh_bytes = convertG1 cmQhigh
  , proof1_bytes  = convertG1 proof1
  , proof2_bytes  = convertG1 proof2
  , a_xi_int      = convertZp a_xi
  , b_xi_int      = convertZp b_xi
  , c_xi_int      = convertZp c_xi
  , s1_xi_int     = convertZp s1_xi
  , s2_xi_int     = convertZp s2_xi
  , f_xi_int      = convertZp f_xi
  , t_xi_int      = convertZp t_xi
  , t_xi'_int     = convertZp t_xi'
  , z1_xi'_int    = convertZp z1_xi'
  , z2_xi'_int    = convertZp z2_xi'
  , h1_xi'_int    = convertZp h1_xi'
  , h2_xi_int     = convertZp h2_xi
  , l_xi          = map (F . convertZp) l_xi
  }
