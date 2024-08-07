module Main where

import           Prelude    

{-
import           Bench.Scripts                               (plonkVerifierScript, symbolicVerifierScript)
import           Bench.Statistics                            (TestSize (..), printHeader, printSizeStatistics)
import           Data.Aeson                                  (decode)
import qualified Data.ByteString.Lazy                        as BL
import           Data.Map                                    (fromList, keys)
import           PlutusLedgerApi.V3                          (Interval, POSIXTime, ScriptContext (..), TxInInfo, TxInfo (..), TxOut, always)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           System.IO                                   (Handle, stdout)
import           Text.Printf                                 (hPrintf)

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..), PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                        (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OffChain               (Contract (..), PlonkN, RowContractJSON, mkInput, mkProof, mkSetup, toContract)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit  (applyArgs)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Types                       (Symbolic)
import           ZkFold.Base.Data.Vector                     (Vector(..))

context :: ScriptContext -- fill up with data
context = ScriptContext
  { scriptContextTxInfo = TxInfo
    { txInfoInputs          = []     :: [TxInInfo]
    , txInfoReferenceInputs = []     :: [TxInInfo]
    , txInfoOutputs         = []     :: [TxOut]
    , txInfoValidRange      = always :: Interval POSIXTime
    }
  }

printCostsSymbolicVerifier :: Handle -> Input PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> IO ()
printCostsSymbolicVerifier h i p ctx = printSizeStatistics h NoSize (symbolicVerifierScript (DatumSymbolic i) (RedeemerSymbolic p) ctx)

printCostsPlonkVerifier :: Handle -> SetupVerify PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> IO ()
printCostsPlonkVerifier h s i p ctx = printSizeStatistics h NoSize (plonkVerifierScript (RedeemerToken s i p) ctx)

printCostsVerifyPlonk :: Handle -> SetupVerify PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> IO ()
printCostsVerifyPlonk h s i p = printSizeStatistics h NoSize (verifyPlonkScript (RedeemerToken s i p))
-}

main :: IO ()
main = do
  pure ()
  {-
  jsonRowContract <- BL.readFile "test-data/raw-contract-data.json"
  let maybeRowContract = decode jsonRowContract :: Maybe RowContractJSON
  
  case maybeRowContract of
    Just rowContract ->
      let Contract{..} = toContract rowContract
          lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => TxId a' -> TxId a -> Bool a
          lockedByTxId (TxId targetId) (TxId txId) = txId == fromConstant targetId

          Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) @Fr (TxId targetId))
          acc = applyArgs ac [targetId]

          (omega, k1, k2) = getParams 5
          inputs   = fromList [(acOutput acc, 1)]
          plonk            = Plonk @32 omega k1 k2 (Vector @1 $ keys inputs) acc x
          setupP           = setupProve @(PlonkN 32) plonk
          setupV           = setupVerify @(PlonkN 32) plonk
          witness          = (PlonkWitnessInput inputs, ps)
          (input', proof') = prove @(PlonkN 32) setupP witness
      in do
        let setup = mkSetup setupV
            input = mkInput input'
            proof = mkProof @32 setup proof'
            h = stdout
        hPrintf h "\n\n"
        hPrintf h "Run plonk verify\n\n"
        printHeader h
        printCostsVerifyPlonk h setup input proof
        hPrintf h "\n\n"
        hPrintf h "\n\n"
        hPrintf h "Run plonk verifier\n\n"
        printHeader h
        printCostsPlonkVerifier h setup input proof context
        hPrintf h "\n\n"
        hPrintf h "\n\n"
        hPrintf h "Run symbolic plonk verifier\n\n"
        printHeader h
        printCostsSymbolicVerifier h input proof context
        hPrintf h "\n\n"
    _ -> print "Could not deserialize"
  -}
