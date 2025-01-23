module Main where

import           Cardano.Api                             hiding (Lovelace, TxOut)
import           Data.Aeson                              (decode)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.Maybe                              (fromJust)
import           PlutusLedgerApi.V1.Value                (lovelaceValue)
import           PlutusLedgerApi.V3                      as V3
import qualified PlutusTx.Builtins.Internal              as BI
import           PlutusTx.Prelude                        (blake2b_224, sortBy)
import           Prelude                                 (Bool (..), Either (..), IO, Maybe (..), Show (..), concat,
                                                          error, putStr, readFile, return, sequenceA, ($), (++), (.),
                                                          (<$>))
import           System.Directory                        (getCurrentDirectory)
import           System.FilePath                         (takeFileName, (</>))

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils
import           ZkFold.Cardano.OnChain.BLS12_381        (toInput)
import           ZkFold.Cardano.UPLC.Common              (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.PlonkVerifierTx     (plonkVerifierTxCompiled)


main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "plonkVerifierTx-balancing" -> ".." </> ".."
        "e2e-test"                  -> ".."
        _                           -> "."

  IdentityCircuitContract x ps <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "symbolic-contract-data.json")

  let (setup, _, _) = identityCircuitVerificationBytes x ps

  let assetsPath = path </> "assets"

  let range   = Interval (LowerBound (NegInf:: Extended POSIXTime) True) (UpperBound PosInf True)
      rangeBD = toBuiltinData range

  -- Inputs
  txin1 <- parseJsonToTxInInfoList [ Nothing ] <$> BL.readFile (assetsPath </> "utxo1.json")
  txin2 <- parseJsonToTxInInfoList [ Just $ plonkVerifierTxCompiled setup ] <$> BL.readFile (assetsPath </> "utxo2.json")
  let inputsE = [txin1, txin2]

  -- Reference inputs
  txin3 <- parseJsonToTxInInfoList [ Just $ parkingSpotCompiled 54 ] <$> BL.readFile (assetsPath </> "utxo3.json")
  let referencesE = [txin3]

  -- Outputs
  addr1T <- readFile (assetsPath </> "alice.addr")
  let val1 = Lovelace 10000000

  let dataE = do
        txIns  <- concat <$> sequenceA inputsE
        txRefs <- concat <$> sequenceA referencesE
        addr1  <- parseAddress addr1T
        return (txIns, txRefs, addr1)

  case dataE of
    Left errMsg                  -> error $ "Error: " ++ errMsg ++ "\n\n"
    Right (txIns, txRefs, addr1) -> do
      let out1   = TxOut addr1 (lovelaceValue val1) NoOutputDatum Nothing
          txOuts = [out1]

      putStr $ "\nInputs:\n" ++ (show txIns) ++ "\n\n"
      putStr $ "Reference inputs:\n" ++ (show txRefs) ++ "\n\n"
      putStr $ "Outputs:\n" ++ (show txOuts) ++ "\n\n"

      let txInsSorted = sortBy (\u v -> outRefCompare (txInInfoOutRef u) (txInInfoOutRef v)) txIns
      let txInsBD  = toBuiltinData txInsSorted
          txRefsBD = toBuiltinData txRefs
          txOutsBD = toBuiltinData txOuts
          txDataBD = mkTuple4 txInsBD txRefsBD txOutsBD rangeBD

      let input = toInput . blake2b_224 . BI.serialiseData $ txDataBD
      putStr $ "Verifier's input: " ++ (show input) ++ "\n\n"

      putStr "Generating proof...\n\n"
      let (_, _, proof) = stateCheckVerificationBytes x ps input
      BS.writeFile (assetsPath </> "redeemerSymbolicVerifier.json") $ prettyPrintJSON $ dataToJSON proof


----- HELPER FUNCTIONS -----

mkTuple4 :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData
mkTuple4 a b c d =
  BI.mkList $
    BI.mkCons a $
      BI.mkCons b $
        BI.mkCons c $
          BI.mkCons d $
            BI.mkNilData BI.unitval
