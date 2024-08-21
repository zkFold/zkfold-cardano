module Main where


import           Bench.Scripts                               (plonkVerifierScript, symbolicVerifierScript, verifyPlonkScript)
import           Bench.Statistics                            (TestSize (..), printHeader, printSizeStatistics)
import           PlutusLedgerApi.V3                          (Interval, POSIXTime, ScriptContext (..), TxInInfo, TxInfo (..), TxOut, always)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           System.IO                                   (Handle, stdout)
import           Text.Printf                                 (hPrintf)

import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                        (PlonkPlutus)

import           Test.QuickCheck.Arbitrary                   (Arbitrary (..))
import           Test.QuickCheck.Gen                         (generate)
import           ZkFold.Cardano.Examples.EqualityCheck       (equalityCheckVerificationBytes)


context :: ScriptContext -- fill up with data
context = ScriptContext
  { scriptContextTxInfo = TxInfo
    { txInfoInputs          = []     :: [TxInInfo]
    , txInfoReferenceInputs = []     :: [TxInInfo]
    , txInfoOutputs         = []     :: [TxOut]
    , txInfoValidRange      = always :: Interval POSIXTime
    }
  }

printCostsSymbolicVerifier :: Handle -> SetupVerify PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> IO ()
printCostsSymbolicVerifier h s p ctx = printSizeStatistics h NoSize (symbolicVerifierScript s p ctx)

printCostsPlonkVerifier :: Handle -> SetupVerify PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> IO ()
printCostsPlonkVerifier h s p ctx = printSizeStatistics h NoSize (plonkVerifierScript s p ctx)

printCostsVerifyPlonk :: Handle -> SetupVerify PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> IO ()
printCostsVerifyPlonk h s i p = printSizeStatistics h NoSize (verifyPlonkScript s i p)


main :: IO ()
main = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary
    
    putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n"

    let (setup, input, proof) = equalityCheckVerificationBytes x ps targetValue
    let h = stdout

    hPrintf h "\n\n"
    hPrintf h "Run plonk verify\n\n"
    printHeader h
    printCostsVerifyPlonk h setup input proof
    hPrintf h "\n\n"
    hPrintf h "\n\n"
    hPrintf h "Run plonk verifier\n\n"
    printHeader h
    printCostsPlonkVerifier h setup proof context
    hPrintf h "\n\n"
    hPrintf h "\n\n"
    hPrintf h "Run symbolic plonk verifier\n\n"
    printHeader h
    printCostsSymbolicVerifier h setup proof context
    hPrintf h "\n\n"

