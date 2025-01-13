module Main where

import           Backend.JsonToData                      (parseJsonToTxInInfoList)
import           Cardano.Api                             hiding (Lovelace)
import           Data.Aeson                              (decode)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.Maybe                              (fromJust)
import           PlutusLedgerApi.V3                      as V3
import           PlutusTx.Builtins.Internal              (serialiseData)
import           PlutusTx.Prelude                        (Ordering (..), blake2b_224, compare, sortBy)
import           Prelude                                 (Either (..), IO, Maybe (..), Show (..), concat, putStr,
                                                          sequenceA, ($), (++), (.), (<$>), (>>))
import           System.Directory                        (getCurrentDirectory)
import           System.Exit                             (exitFailure)
import           System.FilePath                         (takeFileName, (</>))

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (dataToJSON)
import           ZkFold.Cardano.OnChain.BLS12_381        (toInput)
import           ZkFold.Cardano.UPLC.PlonkVerifierTx     (plonkVerifierTxCompiled')


main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "symbolic-balancing" -> ".." </> ".."
        "backends"           -> ".."
        "scripts"            -> ".."
        _                    -> "."

  IdentityCircuitContract x ps <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "symbolic-contract-data.json")

  let (setup, _, _) = identityCircuitVerificationBytes x ps

  let assetsPath = path </> "assets"

  txin1 <- parseJsonToTxInInfoList [Nothing] <$> BL.readFile (assetsPath </> "utxo1.json")
  txin2 <- parseJsonToTxInInfoList [Just $ plonkVerifierTxCompiled' setup] <$> BL.readFile (assetsPath </> "utxo2.json")

  putStr $ "Input UTxO from Alice:\n\n" ++ (show txin1) ++ "\n\n"
  putStr $ "Input UTxO from SymbolicVerifier:\n\n" ++ (show txin2) ++ "\n\n"

  case concat <$> sequenceA [txin1, txin2] of
    Right txins -> do
      let txinsSorted = sortBy (\u v -> outRefCompare (txInInfoOutRef u) (txInInfoOutRef v)) txins

      let txinBD  = toBuiltinData txinsSorted
      putStr $ "Data:\n\n" ++ (show txinBD) ++ "\n\n"

      let txinBBS = serialiseData txinBD
      putStr $ "Serialised data:\n\n" ++ (show txinBBS) ++ "\n\n"

      let input = toInput $ blake2b_224 txinBBS
      putStr $ "Verifier's input: " ++ (show input) ++ "\n\n"

      putStr "Generating proof...\n"

      let (_, _, proof) = stateCheckVerificationBytes x ps input

      BS.writeFile (assetsPath </> "redeemerSymbolicVerifier.json") $ prettyPrintJSON $ dataToJSON proof

    Left errMsg -> putStr ("Error: " ++ errMsg ++ "\n\n") >> exitFailure


----- HELPER FUNCTIONS -----

-- | Compare function for 'TxOutRef'
outRefCompare :: TxOutRef -> TxOutRef -> Ordering
outRefCompare o1 o2 =
    case compare (txOutRefId o1) (txOutRefId o2) of
        EQ  -> compare (txOutRefIdx o1) (txOutRefIdx o2)
        ord -> ord
