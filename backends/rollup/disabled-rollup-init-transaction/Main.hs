module Main where

import           PlutusTx.Builtins                           (byteStringToInteger, mkI)

import           Cardano.Api                             (IsPlutusScriptLanguage, PlutusScriptV3, Script (..),
                                                          hashScript, plutusScriptVersion, prettyPrintJSON,
                                                          serialiseToRawBytes, unsafeHashableScriptData,
                                                          writeFileTextEnvelope)
import           Cardano.Api.Ledger                      (toCBOR)
import           Cardano.Api.Shelley                     (File (..), PlutusScript (..), fromPlutusData,
                                                          scriptDataToJsonDetailedSchema)
import           Codec.CBOR.Write                        (toStrictByteString)
import           Control.Monad                           (void)
import           Data.Aeson                              (encode)
import qualified Data.Aeson                              as Aeson
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import qualified PlutusLedgerApi.V2                      as V2
import qualified PlutusLedgerApi.V3                      as V3
import           PlutusTx                                (CompiledCode, ToData (..))
import           Prelude                                 (Bool (..), Either (..), FilePath, IO, Integer, Maybe (..),
                                                          Show (..), head, print, putStr, read, return, ($), (++), (.),
                                                          (<$>), (==))
import           System.Directory                        (createDirectoryIfMissing, getCurrentDirectory)
import           System.Environment                      (getArgs)
import           System.FilePath                         (takeFileName, (</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)
import           Text.Parsec                             (many1, parse)
import           Text.Parsec.Char                        (digit)
import           Text.Parsec.String                      (Parser)

import           Backend.NFT                             (nftPolicyCompiled)
import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E             (IdentityCircuitContract (..), RollupInfo (..))
import           ZkFold.Cardano.OnChain.BLS12_381        (toInput)
import           ZkFold.Cardano.OnChain.Utils            (dataToBlake)
import           ZkFold.Cardano.UPLC                     (parkingSpotCompiled, rollupCompiled)
import           ZkFold.Cardano.UPLC.Rollup              (RollupRedeemer (..))


main :: IO ()
main = do
  putStr "Hello zkFold!\n"

saveRollupPlutus :: FilePath -> TxId -> IO ()
saveRollupPlutus path nftId = do
  x  <- generate arbitrary
  ps <- generate arbitrary

  let contract = IdentityCircuitContract x ps

  BL.writeFile (path </> "test-data" </> "plonk-raw-contract-data.json") $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n\n"

-- data RollupSetup = RollupSetup
--   { rsLedgerRules  :: SetupBytes
--   , rsDataCurrency :: CurrencySymbol
--   , rsThreadValue  :: Value
--   , rsFeeAddress   :: Address
--   } deriving stock (Show, Generic)

  let (ledgerRules, iniState, _) = identityCircuitVerificationBytes x ps
      nextState                  = byteStringToInteger BigEndian $ dataToBlake (toF iniState, [] :: TxOut, [] :: TxOut,
                                                                                lovelaceValue $ V2.Lovelace 100000000)
      (_, _, proof)              = stateCheckVerificationBytes x ps nextState

  let threadCS    = currencySymbolOf . nftPolicyCompiled $ TxOut (TxId nftId) 0
      threadName  = TokenName (fromString "7a6b466f6c64" :: BuiltinByteString)  -- token name: "zkFold"
      rollupSetup = RollupSetup
                    { rsLedgerRules = ledgerRules
                    , rsDataCurrency = 
                    , rsThreadValue = singleton threadCS threadName 1

-- data RollupRedeemer =
--       UpdateRollup ProofBytes [BuiltinByteString]

  let rollupRedeemer = UpdateRollup proof []

  -- let rollupInfoA = RollupInfo { riNextState = nextState, riRedeemer = redeemerRollup }

  let assetsPath = path </> "assets"

  savePlutus (assetsPath </> "rollup.plutus") $ rollupCompiled ledgerRules

  BS.writeFile (assetsPath </> "datumB.cbor") $ dataToCBOR iniState
  BS.writeFile (assetsPath </> "rollupInfoA.json") $ prettyPrintJSON $ dataToJSON rollupInfoA

saveParkingSpotPlutus :: FilePath -> Integer -> IO ()
saveParkingSpotPlutus path = savePlutus (path </> "assets" </> "parkingSpot.plutus") . parkingSpotCompiled

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "rollup"   -> ".." </> ".."
        "e2e-test" -> ".."
        _          -> "."

  createDirectoryIfMissing True $ path </> "test-data"
  createDirectoryIfMissing True $ path </> "assets"

  nftIdString <- head <$> getArgs
  let nftId = TxId (fromString nftId :: BuiltinByteString)

  saveRollupPlutus path nftId
  saveParkingSpotPlutus path 54
  saveNftPolicyPlutus path nftId

  putStr "\nDone serializing plutus scripts and initializing state.\n\n"

  -- tagE <- parse integerParser "" . head <$> getArgs
  -- case tagE of
  --   Right tag -> do
  --     saveRollupPlutus path
  --     saveParkingSpotPlutus path tag
  --     putStr "\nDone serializing plutus scripts and initializing state.\n\n"
  --   Left err    -> print $ "parse error: " ++ show err


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

-- | Credential of compiled validator script
credentialOf :: CompiledCode a -> V3.Credential
credentialOf = V3.ScriptCredential . V3.ScriptHash . V3.toBuiltin . serialiseToRawBytes . hashScript
               . PlutusScript plutusScriptVersion . PlutusScriptSerialised @PlutusScriptV3 . V3.serialiseCompiledCode

-- | Currency symbol of compiled minting script
currencySymbolOf :: CompiledCode a -> V3.CurrencySymbol
currencySymbolOf = V3.CurrencySymbol . V3.toBuiltin . serialiseToRawBytes . hashScript
               . PlutusScript plutusScriptVersion . PlutusScriptSerialised @PlutusScriptV3 . V3.serialiseCompiledCode

-- | Parser for a positive integer
integerParser :: Parser Integer
integerParser = do
  digits <- many1 digit
  return $ read digits
