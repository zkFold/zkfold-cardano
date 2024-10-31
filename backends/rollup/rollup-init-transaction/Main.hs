module Main where

import           Cardano.Api                              (IsPlutusScriptLanguage, PlutusScriptV3, Script (..),
                                                           hashScript, plutusScriptVersion, prettyPrintJSON,
                                                           serialiseToRawBytes, unsafeHashableScriptData,
                                                           writeFileTextEnvelope)
import           Cardano.Api.Ledger                       (toCBOR)
import           Cardano.Api.Shelley                      (File (..), PlutusScript (..), fromPlutusData,
                                                           scriptDataToJsonDetailedSchema)
import           Codec.CBOR.Write                         (toStrictByteString)
import           Control.Monad                            (void)
import           Data.Aeson                               (encode)
import qualified Data.Aeson                               as Aeson
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Lazy                     as BL
import qualified PlutusLedgerApi.V2                       as V2
import qualified PlutusLedgerApi.V3                       as V3
import           PlutusTx                                 (CompiledCode, ToData (..))
import qualified PlutusTx.Eq                              as Tx
import           Prelude                                  (Bool (..), Either (..), FilePath, IO, Integer, Maybe (..),
                                                           Show (..), head, print, putStr, read, return, ($), (++), (.),
                                                           (<$>), (==))
import           System.Directory                         (createDirectoryIfMissing, getCurrentDirectory)
import           System.Environment                       (getArgs)
import           System.FilePath                          (takeFileName, (</>))
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)
import           Text.Parsec                              (many1, parse)
import           Text.Parsec.Char                         (digit)
import           Text.Parsec.String                       (Parser)

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.Examples.IdentityCircuit  (identityCircuitVerificationBytes,
                                                           stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E              (IdentityCircuitContract (..))
import           ZkFold.Cardano.OnChain.BLS12_381         (F (..), toInput)
import           ZkFold.Cardano.OnChain.Plonk             (PlonkPlutus)
import           ZkFold.Cardano.OnChain.Utils             (dataToBlake)
import           ZkFold.Cardano.UPLC                      (parkingSpotCompiled, rollupCompiled)
import           ZkFold.Cardano.UPLC.Rollup               (RollupRedeemer (..))

saveRollupPlutus :: FilePath -> IO ()
saveRollupPlutus path = do
  x  <- generate arbitrary
  ps <- generate arbitrary

  let contract = IdentityCircuitContract x ps

  BL.writeFile (path </> "test-data" </> "plonk-raw-contract-data.json") $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n\n"

  let (ledgerRules, iniState, _) = identityCircuitVerificationBytes x ps
      nextState                 = toInput $ dataToBlake (iniState, [iniState])
      (_, input, proof)         = stateCheckVerificationBytes x ps nextState
      (_, input', proof')       = stateCheckVerificationBytes x ps (F 0)

  let redeemerRollupA = RollupRedeemer
        { rrProof   = proof
        , rrAddress = V3.Address rollupCredential Nothing
        , rrValue   = lovelace 3000000
        , rrState   = iniState
        , rrUpdate  = [iniState]
        }
        where
          rollupCredential = credentialOf $ rollupCompiled ledgerRules
          lovelace         = V2.singleton V2.adaSymbol V2.adaToken

  let assetsPath = path </> "assets"

  savePlutus (assetsPath </> "rollup.plutus") $ rollupCompiled ledgerRules

  BS.writeFile (assetsPath </> "datumRollupA.cbor") $ dataToCBOR iniState
  BS.writeFile (assetsPath </> "redeemerRollupA.cbor") $ dataToCBOR redeemerRollupA
  BS.writeFile (assetsPath </> "redeemerRollupA.json") $ prettyPrintJSON $ dataToJSON redeemerRollupA

  putStr $ "input == nextState: " ++ (show $ input Tx.== nextState) ++ ".\n"
  putStr $ "\nVerifies proof: " ++ (show $ verify @PlonkPlutus @HaskellCore ledgerRules input proof) ++ ".\n"
  putStr $ "\nVerifies proof assuming nextState is zero: " ++ (show $ verify @PlonkPlutus @HaskellCore ledgerRules input' proof') ++ ".\n"

saveParkingSpotPlutus :: FilePath -> Integer -> IO ()
saveParkingSpotPlutus path = savePlutus (path </> "assets" </> "parkingSpot.plutus") . parkingSpotCompiled

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let currentDirName = takeFileName currentDir
      path           = if currentDirName == "rollup" then "../.."
                          else if currentDirName == "e2e-test" then ".." else "."

  createDirectoryIfMissing True $ path </> "test-data"
  createDirectoryIfMissing True $ path </> "assets"

  tagE <- parse integerParser "" . head <$> getArgs

  case tagE of
    Right tag -> do
      saveRollupPlutus path
      saveParkingSpotPlutus path tag

      BS.writeFile (path </> "assets" </> "unit.cbor") $ dataToCBOR ()

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
