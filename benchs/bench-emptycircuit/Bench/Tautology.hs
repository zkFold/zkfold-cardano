module Bench.Tautology (tautologyCheckVerificationBytes) where

import ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import ZkFold.Base.Protocol.Plonkup.Prover.Secret
import ZkFold.Cardano.Examples.EmptyCircuit        (emptyCircuitVerificationBytes)
import ZkFold.Cardano.OnChain.Plonk.Data           (InputBytes, ProofBytes, SetupBytes)


tautologyCheckVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1 -> Fr -> (SetupBytes, InputBytes, ProofBytes)
tautologyCheckVerificationBytes x ps _targetValue = emptyCircuitVerificationBytes x ps
