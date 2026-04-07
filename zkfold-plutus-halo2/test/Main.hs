{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import           Control.Exception                      (evaluate)
import           Control.Monad.Except                   (runExceptT)
import           Data.Binary                            (Binary)
import           Data.Foldable                          (toList)
import           Data.Functor.Rep                       (Representable)
import           GHC.Generics                           (U1 (..), (:*:) (..))
import           GHC.TypeNats                           (KnownNat)
import           Plutus.Crypto.BlsTypes                 (mkScalar)
import           Plutus.Crypto.Halo2.Generic.Verifier   (verify)
import qualified PlutusTx.Builtins                      as PlutusTx
import           System.Environment                     (getEnv)
import           Test.Hspec                             (Spec, hspec, it, shouldBe)

import           ZkFold.Algebra.Class                   (toConstant)
import           ZkFold.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Algebra.Polynomial.Univariate   (PolyVec)
import           ZkFold.ArithmeticCircuit               (ArithmeticCircuit, acSizeM, acSizeN)
import           ZkFold.Protocol.Halo2.Export           (runProver)
import           ZkFold.Protocol.Plonkup.Relation       (PlonkupRelation (..), toPlonkupRelation)
import           ZkFold.Symbolic.Data.Class             (arithmetize, payload)
import           ZkFold.Symbolic.Interpreter            (runInterpreter)
import           ZkFold.Symbolic.Ledger.Circuit.Compile (LedgerCircuit, LedgerCircuitGates, LedgerContractCompiledInput,
                                                         LedgerContractInput (..), LedgerContractOutputLayout,
                                                         ledgerCircuit)
import           ZkFold.Symbolic.Ledger.Examples.One
import qualified ZkFold.Symbolic.Ledger.Examples.One    as One
import           ZkFold.Symbolic.Ledger.Types
import           ZkFold.Symbolic.Ledger.Types.Field     (RollupBF, RollupBFInterpreter)


main :: IO ()
main = hspec specHalo2E2EOne



extractLedgerPublicInputs
  :: forall bi bo ud a i o t.
     ( KnownNat bi
     , KnownNat bo
     , KnownNat ud
     , KnownNat a
     , KnownNat i
     , KnownNat o
     , KnownNat t
     , SignatureState bi bo ud a RollupBFInterpreter
     , SignatureTransactionBatch ud i o a t RollupBFInterpreter
     )
  => LedgerCircuit bi bo ud a i o t
  -> LedgerContractInput bi bo ud a i o t RollupBFInterpreter
  -> Maybe [RollupBF]
extractLedgerPublicInputs circuit input = do
  rel <-
    toPlonkupRelation
      @(LedgerContractCompiledInput bi bo ud a i o t)
      @(LedgerContractOutputLayout bi bo a)
      @LedgerCircuitGates
      @RollupBF
      @(PolyVec RollupBF)
      circuit

  let witnessInputs =
        runInterpreter $ arithmetize input

      paddedWitnessInputs :: LedgerContractCompiledInput bi bo ud a i o t RollupBF
      paddedWitnessInputs =
        (witnessInputs :*: U1) :*: (payload input :*: U1)

  pure $ pubInput rel paddedWitnessInputs

specHalo2E2EOne :: Spec
specHalo2E2EOne =
  it "E2E ledger circuit, One: prove and verify" $ do
    let lci :: LedgerContractInput Bi Bo Ud A Ixs Oxs TxCount I
        lci =
          LedgerContractInput
            { lciPreviousState = prevState
            , lciTransactionBatch = batch
            , lciNewState = newState
            , lciStateWitness = One.witness
            }
    proverExe <- getEnv "HALO2_PROVER"
    compiledCircuit <- evaluate $ ledgerCircuit @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @I

    putStrLn $
      "constraints: " <> show (acSizeN compiledCircuit) <> ", variables: " <> show (acSizeM compiledCircuit)

    let witnessInputs = runInterpreter $ arithmetize lci
        compiledInput = (witnessInputs :*: U1) :*: (payload lci :*: U1)

    putStrLn "Computing proof"

    Right zkLedgerProof <- runExceptT $ runProver @_ @_ @LedgerCircuitGates @_ @(PolyVec RollupBF) proverExe compiledCircuit compiledInput

    putStrLn "Proof computed"

    let Just inputs = extractLedgerPublicInputs compiledCircuit lci
    let sc i = mkScalar (fromIntegral $ toConstant $ inputs !! i)

    print inputs

    let verifierResult = verify (PlutusTx.toBuiltin zkLedgerProof) (sc 0) (sc 1) (sc 2) (sc 3) (sc 4) (sc 5) (sc 6) (sc 7) (sc 8) (sc 9) (sc 10) (sc 11) (sc 12) (sc 13) (sc 14) (sc 15) (sc 16) (sc 17) (sc 18)

    print verifierResult

    fst verifierResult `shouldBe` True
