module ZkFoldBenchmark.Verifier.RunVerifier where

import           Data.Aeson                       (decode)
import qualified Data.ByteString.Lazy             as BL
import           Data.Word                        ()
import           System.IO                        (Handle)
import           Text.Printf                      (hPrintf)

import           ZkFold.Cardano.Plonk.Inputs
import           ZkFoldBenchmark.Common           (TestSize (..), printHeader, printSizeStatistics)
import           ZkFoldBenchmark.Verifier.Scripts (verifyPlonkScript, verifySymbolicScript)

printCostsVerifySymbolicScript :: Handle -> SetupPlonkPlutus -> InputPlonkPlutus -> ProofPlonkPlutus -> IO ()
printCostsVerifySymbolicScript h s i p = printSizeStatistics h NoSize (verifySymbolicScript s i p)

printCostsVerifierPlonk :: Handle -> SetupPlonkPlutus -> InputPlonkPlutus -> ProofPlonkPlutus -> IO ()
printCostsVerifierPlonk h s i p = printSizeStatistics h NoSize (verifyPlonkScript s i p)

runVerifier :: Handle -> IO ()
runVerifier h = do
    jsonDataProof <- BL.readFile "test-data/proof.json"
    jsonDataSetup <- BL.readFile "test-data/setup.json"
    jsonDataInput <- BL.readFile "test-data/input.json"
    let maybeProof = decode jsonDataProof :: Maybe ProofJSON
    let maybeSetup = decode jsonDataSetup :: Maybe SetupJSON
    let maybeInput = decode jsonDataInput :: Maybe InputJSON
    case (maybeProof, maybeSetup, maybeInput) of
      (Just prf, Just stp, Just inp) -> do
        let p = convertProofPlonkPlutus prf
        let s = convertSetupPlonkPlutus stp
        let i = convertInputPlonkPlutus inp
        hPrintf h "\n\n"
        hPrintf h "Run plonk verifier\n\n"
        printHeader h
        printCostsVerifierPlonk h s i p
        hPrintf h "\n\n"
        hPrintf h "\n\n"
        hPrintf h "Run symbolic plonk verifier\n\n"
        printHeader h
        printCostsVerifySymbolicScript h s i p
        hPrintf h "\n\n"
      _ -> print "Could not deserialize"
