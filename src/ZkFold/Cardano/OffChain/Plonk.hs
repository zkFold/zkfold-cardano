{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.OffChain.Plonk where

import           PlutusTx.Builtins
import           PlutusTx.Prelude                                  (($), (.))
import           Prelude                                           (fromIntegral)

import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381       (BLS12_381_G1, BLS12_381_G2)
import qualified ZkFold.Base.Data.Vector                           as V
import           ZkFold.Base.Protocol.NonInteractiveProof          (NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonk
import           ZkFold.Base.Protocol.Plonkup.Input
import           ZkFold.Base.Protocol.Plonkup.Proof
import           ZkFold.Base.Protocol.Plonkup.Verifier.Commitments
import           ZkFold.Base.Protocol.Plonkup.Verifier.Setup
import           ZkFold.Cardano.OffChain.ECC                       (convertG1, convertG2, convertZp)
import           ZkFold.Cardano.OnChain.BLS12_381
import           ZkFold.Cardano.OnChain.Plonk.Data                 (InputBytes, ProofBytes (..), SetupBytes (..))
import           ZkFold.Prelude                                    (log2ceiling)

--------------- Transform Plonk Base to Plonk BuiltinByteString ----------------

type PlonkN i n = Plonk i n 1 BLS12_381_G1 BLS12_381_G2 BuiltinByteString

mkSetup :: forall i n . KnownNat n => SetupVerify (PlonkN i n) -> SetupBytes
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
    , cmS1_bytes = convertG1 cmS1
    , cmS2_bytes = convertG1 cmS2
    , cmS3_bytes = convertG1 cmS3
    }

mkInput :: Input (PlonkN i n) -> InputBytes
mkInput (PlonkupInput input) = F . convertZp $ V.head input

mkProof :: Proof (PlonkN i n) -> ProofBytes
mkProof PlonkupProof {..} = ProofBytes
  { cmA_bytes     = convertG1 cmA
  , cmB_bytes     = convertG1 cmB
  , cmC_bytes     = convertG1 cmC
  , cmZ1_bytes    = convertG1 cmZ1
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
  , z1_xi'_int    = convertZp z1_xi'
  , l1_xi         = F $ convertZp l1_xi
  }
