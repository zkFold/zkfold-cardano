{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import ZkFold.Symbolic.Ledger.Examples.One
import ZkFold.Symbolic.Ledger.Examples.One qualified as One

import Control.Exception (evaluate)
import Control.Monad.Except (runExceptT)
import GHC.Generics (U1 (..), (:*:) (..))
import Test.Hspec (Spec, it, hspec, shouldBe)
import ZkFold.ArithmeticCircuit (acSizeM, acSizeN)
import ZkFold.Protocol.Halo2.Export (runProver)
import ZkFold.Symbolic.Data.Class (arithmetize, payload)
import ZkFold.Symbolic.Interpreter (runInterpreter)
import System.Environment (getEnv)

import Data.Foldable (toList)
import Data.Functor.Rep (Representable)
import Data.Binary (Binary)
import GHC.Generics (U1(..), (:*:)(..))
import GHC.TypeNats (KnownNat)

import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.Algebra.Class (toConstant)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit)
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation(..), toPlonkupRelation)
import ZkFold.Symbolic.Ledger.Circuit.Compile
  ( LedgerCircuit
  , LedgerCircuitGates
  , LedgerContractCompiledInput
  , LedgerContractOutputLayout
  , LedgerContractInput
  )

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field (RollupBF, RollupBFInterpreter)
import PlutusTx.Builtins qualified as PlutusTx

import ZkFold.Symbolic.Ledger.Types.Field (RollupBF)
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.Symbolic.Ledger.Circuit.Compile (
  LedgerCircuitGates,
  LedgerContractInput (..),
  ledgerCircuit,
 )
import Plutus.Crypto.Halo2.Generic.Verifier (verify) 
import Plutus.Crypto.BlsTypes (mkScalar)


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
