module Main where

import           Backend.JsonToData                      (parseAddress, parseJsonToTxInInfoList)
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
import qualified Data.Text.IO                            as TIO
import           PlutusLedgerApi.V1.Value                (lovelaceValue)
import           PlutusLedgerApi.V3                      as V3
import           PlutusTx                                (CompiledCode)
import           PlutusTx.Builtins.Internal              (serialiseData)
import qualified PlutusTx.Builtins.Internal              as BI
import           PlutusTx.Prelude                        (Bool (..), Ordering (..), blake2b_224, compare, concat, sortBy)
import           Prelude                                 (Either (..), Maybe (..), Show (..), error, return, sequenceA,
                                                          ($), (++), (.), (<$>))
import           System.Directory                        (getCurrentDirectory)
import           System.FilePath                         (takeFileName, (</>))
import           System.IO                               as IO

import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E             (IdentityCircuitContract (..))
import           ZkFold.Cardano.OnChain.BLS12_381        (toInput)
import           ZkFold.Cardano.UPLC                     (parkingSpotCompiled, plonkVerifierTxCompiled)


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
  addr1T <- TIO.readFile (assetsPath </> "alice.addr")
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

      let input = toInput . blake2b_224 . serialiseData $ txDataBD
      putStr $ "Verifier's input: " ++ (show input) ++ "\n\n"

      putStr "Generating proof...\n\n"
      let (_, _, proof) = stateCheckVerificationBytes x ps input
      BS.writeFile (assetsPath </> "redeemerSymbolicVerifier.json") $ prettyPrintJSON $ dataToJSON proof


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

-- | Compare function for 'TxOutRef'
outRefCompare :: TxOutRef -> TxOutRef -> Ordering
outRefCompare o1 o2 =
    case compare (txOutRefId o1) (txOutRefId o2) of
        EQ  -> compare (txOutRefIdx o1) (txOutRefIdx o2)
        ord -> ord

mkTuple4 :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData
mkTuple4 a b c d =
  BI.mkList $
    BI.mkCons a $
      BI.mkCons b $
        BI.mkCons c $
          BI.mkCons d $
            BI.mkNilData BI.unitval
