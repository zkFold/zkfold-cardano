module Main where

import           Cardano.Api                           (IsPlutusScriptLanguage, PlutusScriptV3, Script (..), hashScript,
                                                        plutusScriptVersion, prettyPrintJSON, serialiseToRawBytes,
                                                        unsafeHashableScriptData, writeFileTextEnvelope)
import           Cardano.Api.Ledger                    (toCBOR)
import           Cardano.Api.Shelley                   (File (..), PlutusScript (..), fromPlutusData,
                                                        scriptDataToJsonDetailedSchema)
import           Codec.CBOR.Write                      (toStrictByteString)
import           Control.Monad                         (void)
import           Data.Aeson                            (encode)
import qualified Data.Aeson                            as Aeson
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as BL
import qualified PlutusLedgerApi.V2                    as V2
import qualified PlutusLedgerApi.V3                    as V3
import           PlutusTx                              (CompiledCode, ToData (..))
import           Prelude                               (Bool (..), Either (..), FilePath, IO, Integer, Maybe (..),
                                                        Show (..), head, print, putStr, read, return, ($), (++), (.),
                                                        (<$>))
import           System.Directory                      (createDirectoryIfMissing)
import           System.Environment                    (getArgs)
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)
import           Text.Parsec                           (many1, parse)
import           Text.Parsec.Char                      (digit)
import           Text.Parsec.String                    (Parser)

import           ZkFold.Cardano.Examples.EqualityCheck (equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E           (EqualityCheckContract (..))
import           ZkFold.Cardano.UPLC                   (parkingSpotCompiled, rollupCompiled')
import           ZkFold.Cardano.UPLC.Rollup            (RollupRedeemer (..))


saveRollupPlutus :: IO ()
saveRollupPlutus = do
  x           <- generate arbitrary
  ps          <- generate arbitrary
  targetValue <- generate arbitrary

  let contract = EqualityCheckContract x ps targetValue

  BL.writeFile "../../test-data/plonk-raw-contract-data.json" $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n\n"

  let (ledgerRules, iniState, proof) = equalityCheckVerificationBytes x ps targetValue

  let redeemerRollupA = RollupRedeemer
        { rrProof   = proof
        , rrAddress = V3.Address rollupCredential Nothing
        , rrValue   = lovelace 3000000
        , rrState   = iniState
        , rrUpdate  = [iniState]
        }
        where
          rollupCredential = credentialOf $ rollupCompiled' ledgerRules
          lovelace         = V2.singleton V2.adaSymbol V2.adaToken

  savePlutus "../../assets/rollup.plutus" $ rollupCompiled' ledgerRules

  BS.writeFile "../../assets/datumRollupA.cbor" $ dataToCBOR iniState
  BS.writeFile "../../assets/redeemerRollupA.cbor" $ dataToCBOR redeemerRollupA
  BS.writeFile "../../assets/redeemerRollupA.json" $ prettyPrintJSON $ dataToJSON redeemerRollupA

saveParkingSpotPlutus :: Integer -> IO ()
saveParkingSpotPlutus = savePlutus "../../assets/parkingSpot.plutus" . parkingSpotCompiled

main :: IO ()
main = do
  createDirectoryIfMissing True "../../test-data"
  createDirectoryIfMissing True "../../assets"

  tagE <- parse integerParser "" . head <$> getArgs

  case tagE of
    Right tag -> do
      saveRollupPlutus
      saveParkingSpotPlutus tag

      BS.writeFile "../../assets/unit.cbor" $ dataToCBOR ()

      putStr "\nDone serializing plutus scripts and initializing state.\n\n"

    Left err    -> print $ "parse error: " ++ show err


----- HELPER FUNCTIONS -----

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . V3.toData

-- | Write serialized script to a file.
writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

-- | Serialize plutus script
savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . V3.serialiseCompiledCode

-- | Credential of compiled script
credentialOf :: CompiledCode a -> V3.Credential
credentialOf = V3.ScriptCredential . V3.ScriptHash . V3.toBuiltin . serialiseToRawBytes . hashScript
               . PlutusScript plutusScriptVersion . PlutusScriptSerialised @PlutusScriptV3 . V3.serialiseCompiledCode

-- | Parser for a positive integer
integerParser :: Parser Integer
integerParser = do
  digits <- many1 digit
  return $ read digits
