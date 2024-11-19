module Main where

import           Backend.JsonToData                      (parseJsonToTxInInfoList)
import           Cardano.Api                             (IsPlutusScriptLanguage, PlutusScriptV3, prettyPrintJSON,
                                                          unsafeHashableScriptData, writeFileTextEnvelope)
import           Cardano.Api.Ledger                      (toCBOR)
import           Cardano.Api.Shelley                     (File (..), PlutusScript (..), fromPlutusData,
                                                          scriptDataToJsonDetailedSchema)
import           Codec.CBOR.Write                        (toStrictByteString)
import           Control.Monad                           (void)
import           Data.Aeson                              (decode)
import qualified Data.Aeson                              as Aeson
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.Maybe                              (fromJust)
import qualified PlutusLedgerApi.V3                      as V3
import           PlutusTx                                (CompiledCode, ToData (..))
import           PlutusTx.Builtins.Internal              (serialiseData)
import           PlutusTx.Prelude                        (blake2b_224, head)
import           Prelude                                 (Either (..), FilePath, IO, Maybe (..), Show (..), concat,
                                                          putStr, sequenceA, ($), (++), (.), (<$>))
import           System.Directory                        (getCurrentDirectory)
import           System.FilePath                         (takeFileName, (</>))

import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E             (IdentityCircuitContract (..))
import           ZkFold.Cardano.OnChain.BLS12_381        (toInput)
import           ZkFold.Cardano.UPLC                     (symbolicVerifierCompiled')


main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "symbolic-balancing" -> ".." </> ".."
        "backends"           -> ".."
        "e2e-test"           -> ".."
        _                    -> "."

  IdentityCircuitContract x ps <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "plonk-raw-contract-data.json")

  let (setup, _, _) = identityCircuitVerificationBytes x ps

  let assetsPath = path </> "assets"

  txin1 <- parseJsonToTxInInfoList [Nothing] <$> BL.readFile (assetsPath </> "utxo1.json")
  txin2 <- parseJsonToTxInInfoList [Just $ symbolicVerifierCompiled' setup] <$> BL.readFile (assetsPath </> "utxo2.json")

  putStr $ "Input UTxO from Alice:\n\n" ++ (show txin1) ++ "\n\n"
  putStr $ "Input UTxO from SymbolicVerifier:\n\n" ++ (show txin2) ++ "\n\n"

  case concat <$> sequenceA [txin1, txin2] of
    Right txins -> do
      let txinBD  = toBuiltinData . head $ txins
      putStr $ "Data:\n\n" ++ (show txinBD) ++ "\n\n"

      let txinBBS = serialiseData txinBD
      putStr $ "Serialised data:\n\n" ++ (show txinBBS) ++ "\n\n"

      let input = toInput $ blake2b_224 txinBBS
      putStr $ "Verifier's input: " ++ (show input) ++ "\n\n"

      putStr "Generating proof...\n"

      let (_, _, proof) = stateCheckVerificationBytes x ps input

      BS.writeFile (assetsPath </> "redeemerSymbolicVerifier.json") $ prettyPrintJSON $ dataToJSON proof

    Left errMsg -> putStr $ "Error: " ++ errMsg ++ "\n\n"


----- HELPER FUNCTIONS -----

-- | Write serialized script to a file.
writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

-- | Serialize plutus script.
savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . V3.serialiseCompiledCode

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . V3.toData
