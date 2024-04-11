{-# OPTIONS_GHC -Wno-name-shadowing #-}
module ZkFoldBenchmark.Verifier.RunVerifier (runVerifier) where

import           Data.Aeson                                  (decode)
import qualified Data.ByteString.Lazy                        as BL
import           Data.Map                                    (fromList)
import           Data.Word                                   ()
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           System.IO                                   (Handle)
import           Text.Printf                                 (hPrintf)

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..), PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                        (Plonk32, PlonkPlutus, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.Plonk.Inputs                 (Contract (..), RowContractJSON, toContract)
import           ZkFold.Symbolic.Cardano.Types.Tx            (TxId (..))
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Types                       (Symbolic)
import           ZkFoldBenchmark.Common                      (TestSize (..), printHeader, printSizeStatistics)
import           ZkFoldBenchmark.Verifier.Scripts            (verifyPlonkScript, verifySymbolicScript)

lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => TxId a' -> TxId a -> () -> Bool a
lockedByTxId (TxId targetId) (TxId txId) _ = txId == fromConstant targetId

printCostsVerifySymbolicScript :: Handle -> Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> IO ()
printCostsVerifySymbolicScript h s i p = printSizeStatistics h NoSize (verifySymbolicScript s i p)

printCostsVerifierPlonk :: Handle -> Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> IO ()
printCostsVerifierPlonk h s i p = printSizeStatistics h NoSize (verifyPlonkScript s i p)

runVerifier :: Handle -> IO ()
runVerifier h = do
  jsonRowContract <- BL.readFile "test-data/rowcontract.json"
  let maybeRowContract = decode jsonRowContract :: Maybe RowContractJSON
  case maybeRowContract of
    Just rowContract ->
      let Contract{..} = toContract rowContract
          Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) (TxId targetId))
          (omega, k1, k2) = getParams 5
          inputs  = fromList [(1, targetId), (acOutput ac, 1)]
          plonk   = Plonk omega k1 k2 inputs ac x
          setup'  = setup @Plonk32 plonk
          w       = (PlonkWitnessInput inputs, ps)
          (input', proof') = prove @Plonk32 setup' w
      in do
        let setup = mkSetup setup'
            input = mkInput input'
            proof = mkProof proof'
        hPrintf h "\n\n"
        hPrintf h "Run plonk verifier\n\n"
        printHeader h
        printCostsVerifierPlonk h setup input proof
        hPrintf h "\n\n"
        hPrintf h "\n\n"
        hPrintf h "Run symbolic plonk verifier\n\n"
        printHeader h
        printCostsVerifySymbolicScript h setup input proof
        hPrintf h "\n\n"
    _ -> print "Could not deserialize"
