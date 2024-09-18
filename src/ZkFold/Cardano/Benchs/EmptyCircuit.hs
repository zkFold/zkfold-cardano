module ZkFold.Cardano.Benchs.EmptyCircuit where

import           Data.Map                                    (fromList)
import           GHC.Generics                                (Par1 (..))
import           GHC.Natural                                 (Natural)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import qualified ZkFold.Base.Data.Vector                     as V
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..), PlonkProverSecret, PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk.OffChain               (PlonkN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.Plonk.OnChain.Data           (InputBytes, ProofBytes, SetupBytes)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compileForceOne)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..), true)

type PlonkBase32 = PlonkN 32

tautology :: forall c. Symbolic c => Bool c
tautology = true

tautologyVerificationBytes :: Fr -> PlonkProverSecret BLS12_381_G1 -> Fr -> (SetupBytes, InputBytes, ProofBytes)
tautologyVerificationBytes x ps targetValue =
    let Bool ac = compileForceOne @Fr (tautology @(ArithmeticCircuit Fr)) :: Bool (ArithmeticCircuit Fr)

        (omega, k1, k2) = getParams 32
        witnessInputs  = fromList [(1, targetValue), (unPar1 $ acOutput ac, 1)]
        indexTargetValue = V.singleton (1 :: Natural)
        plonk   = Plonk @32 omega k1 k2 indexTargetValue ac x
        setupP  = setupProve @PlonkBase32 plonk
        setupV  = setupVerify @PlonkBase32 plonk
        witness = (PlonkWitnessInput witnessInputs, ps)
        (input, proof) = prove @PlonkBase32 setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

