module ZkFold.Cardano.Benchs.EmptyCircuit where

import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Data.Vector                     (Vector (..), unsafeToVector)
import qualified ZkFold.Base.Data.Vector                     as V
import           ZkFold.Base.Protocol.NonInteractiveProof    (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonk
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams)
import           ZkFold.Base.Protocol.Plonkup.Witness
import           ZkFold.Cardano.OffChain.Plonk               (PlonkNZ, mkInput', mkProof', mkSetup')
import           ZkFold.Cardano.OnChain.Plonk()
import           ZkFold.Cardano.OnChain.Plonk.Data           (InputBytes, ProofBytes, SetupBytes)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..))


tautologyVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1 -> Fr -> (SetupBytes, InputBytes, ProofBytes)
tautologyVerificationBytes x ps targetValue =
    let emptyC = mempty {acOutput = V.empty} :: ArithmeticCircuit Fr (Vector 1) (Vector 0)

        (omega, k1, k2) = getParams 32
        witnessInputs  = unsafeToVector [targetValue]
        plonk   = Plonk omega k1 k2 emptyC x :: PlonkNZ 1 32
        setupP  = setupProve @_ @HaskellCore plonk
        setupV  = setupVerify @_ @HaskellCore plonk
        witness = (PlonkupWitnessInput witnessInputs, ps)
        (input, proof) = prove @(PlonkNZ 1 32) @HaskellCore setupP witness

    in (mkSetup' setupV, mkInput' input, mkProof' proof)

