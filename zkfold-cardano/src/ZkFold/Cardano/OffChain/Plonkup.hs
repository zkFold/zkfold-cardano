{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.OffChain.Plonkup where

import           Data.Foldable                                (Foldable, toList)
import           PlutusTx.Builtins                            (BuiltinByteString)
import           PlutusTx.Prelude                             (($), (.))
import           Prelude                                      (fromIntegral, map)

import           ZkFold.Algebra.EllipticCurve.BLS12_381       (BLS12_381_G1_Point, BLS12_381_G2_Point, Fr)
import           ZkFold.Algebra.Number                        (KnownNat, value)
import           ZkFold.Algebra.Polynomial.Univariate         (PolyVec)
import           ZkFold.Cardano.OffChain.BLS12_381            (convertG1, convertG2, convertZp)
import           ZkFold.Cardano.OnChain.BLS12_381.F           (F (..))
import           ZkFold.Cardano.OnChain.Plonkup.Data          (InputBytes, ProofBytes (..), SetupBytes (..))
import           ZkFold.Prelude                               (log2ceiling)
import           ZkFold.Protocol.NonInteractiveProof          (NonInteractiveProof (..))
import           ZkFold.Protocol.Plonkup                      (Plonkup)
import           ZkFold.Protocol.Plonkup.Input                (PlonkupInput (..))
import           ZkFold.Protocol.Plonkup.Proof
import           ZkFold.Protocol.Plonkup.Verifier.Commitments
import           ZkFold.Protocol.Plonkup.Verifier.Setup

--------------- Transform Plonk Base to Plonk BuiltinByteString ----------------

type PlonkupN i o p n = Plonkup i o p n BLS12_381_G1_Point BLS12_381_G2_Point BuiltinByteString (PolyVec Fr)

mkSetup :: forall i o p n . KnownNat n => SetupVerify (PlonkupN i o p n) -> SetupBytes
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

mkInput :: Foldable o => Input (PlonkupN i o p n) -> InputBytes
mkInput (PlonkupInput input) = map (F . convertZp) $ toList input

mkProof :: Proof (PlonkupN i o p n) -> ProofBytes
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
