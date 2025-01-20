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
import           PlutusLedgerApi.V1.Value                (lovelaceValueOf)
import           PlutusLedgerApi.V3                      as V3
import           PlutusTx                                (CompiledCode)
import           PlutusTx.Builtins.Internal              (chooseData, serialiseData, unsafeDataAsList)
import qualified PlutusTx.Builtins.Internal              as BI (head)
-- import           PlutusTx.Prelude                        (Ordering (..), blake2b_224, compare, sortBy)
import           PlutusTx.Prelude                        (Integer, blake2b_224, concat, drop, head, take, length)
-- import           Prelude                                 (Either (..), FilePath, IO, Maybe (..), Show (..), concat,
import           Prelude                                 (Either (..), FilePath, IO, Maybe (..), Show (..),
                                                          putStr, sequenceA, ($), (++), (.), (<$>), (>>))
import           System.Directory                        (getCurrentDirectory)
import           System.Exit                             (exitFailure)
import           System.FilePath                         (takeFileName, (</>))

import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E             (IdentityCircuitContract (..))
import           ZkFold.Cardano.OnChain.BLS12_381        (toInput)
import           ZkFold.Cardano.UPLC                     (parkingSpotCompiled, plonkVerifierTxCompiled')
-- import           ZkFold.Cardano.UPLC.PlonkVerifierTx     (PlonkRedeemer (..))


main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "symbolic-balancing" -> ".." </> ".."
        "backends"           -> ".."
        "e2e-test"           -> ".."
        _                    -> "."

  IdentityCircuitContract x ps <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "symbolic-contract-data.json")

  let (setup, _, _) = identityCircuitVerificationBytes x ps

  let assetsPath = path </> "assets"

  txin1 <- parseJsonToTxInInfoList [Nothing] <$> BL.readFile (assetsPath </> "utxo1.json")
  txin2 <- parseJsonToTxInInfoList [Just $ plonkVerifierTxCompiled' setup] <$> BL.readFile (assetsPath </> "utxo2.json")
  txin3 <- parseJsonToTxInInfoList [Just $ parkingSpotCompiled 54] <$> BL.readFile (assetsPath </> "utxo3.json")

  putStr $ "Input UTxO from Alice:\n\n" ++ (show txin1) ++ "\n\n"
  putStr $ "Input UTxO from SymbolicVerifier:\n\n" ++ (show txin2) ++ "\n\n"
  putStr $ "Input UTxO from ParkingSpot:\n\n" ++ (show txin3) ++ "\n\n"

  -- case concat <$> sequenceA [txin1, txin2] of
  --   Right txins -> do
  --     let txinsSorted = sortBy (\u v -> outRefCompare (txInInfoOutRef u) (txInInfoOutRef v)) txins
  --     let txinBD  = toBuiltinData txinsSorted
  --     putStr $ "Data:\n\n" ++ (show txinBD) ++ "\n\n"
--  case concat <$> sequenceA [txin2, txin1, txin3] of  -- Note order.
--  case concat <$> sequenceA [txin2, txin1] of
  case concat <$> sequenceA [txin2, txin1] of
    Right txIns -> case concat <$> sequenceA [txin3] of
      Right txRefs -> do
        let txLists = [txIns, txRefs]

        -- putStr $ (show $ getLovelace . lovelaceValueOf . txOutValue . txInInfoResolved . head $ txins) ++ "\n"
        -- putStr $ (show $ txOutDatum . txInInfoResolved . head $ txins) ++ "\n\n"
        putStr $ (show txLists) ++ "\n"

        let txDataBD = toBuiltinData txLists  -- --$ [take 2 txins, drop 2 txins]
        putStr $ "Data:\n\n" ++ (show txDataBD) ++ "\n\n"
        -- putStr $ show $ (\d -> chooseData d (0 :: Integer) 1 2 3 4) $ BI.head $ unsafeDataAsList txDataBD

        let txDataBBS = serialiseData txDataBD
        putStr $ "\n\nSerialised data:\n\n" ++ (show txDataBBS) ++ "\n\n"

        let input = toInput $ blake2b_224 txDataBBS
        putStr $ "Verifier's input: " ++ (show input) ++ "\n\n"

        putStr "Generating proof...\n"

        let (_, _, proof) = stateCheckVerificationBytes x ps input

        BS.writeFile (assetsPath </> "redeemerSymbolicVerifier.json") $ prettyPrintJSON $ dataToJSON proof

        -- let redeemerSymbolicVerifier = PlonkRedeemer proof txins
        -- BS.writeFile (assetsPath </> "redeemerSymbolicVerifier.json") $ prettyPrintJSON $ dataToJSON redeemerSymbolicVerifier

    Left errMsg -> putStr ("Error: " ++ errMsg ++ "\n\n") >> exitFailure


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

-- -- | Compare function for 'TxOutRef'
-- outRefCompare :: TxOutRef -> TxOutRef -> Ordering
-- outRefCompare o1 o2 =
--     case compare (txOutRefId o1) (txOutRefId o2) of
--         EQ  -> compare (txOutRefIdx o1) (txOutRefIdx o2)
--         ord -> ord
