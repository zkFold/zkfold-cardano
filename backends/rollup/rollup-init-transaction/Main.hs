module Main where

import           Backend.NFT                             (nftPolicyCompiled)
import           Cardano.Api
import           Cardano.Api.Ledger                      (toCBOR)
import           Cardano.Api.Shelley                     (PlutusScript (..), fromPlutusData,
                                                          scriptDataToJsonDetailedSchema,
                                                          shelleyPayAddrToPlutusPubKHash)
import           Codec.CBOR.Write                        (toStrictByteString)
import           Control.Monad                           (void)
import           Data.Aeson                              (encode)
import qualified Data.Aeson                              as Aeson
import           Data.Bifunctor                          (first)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.String                             (IsString (fromString))
import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as TE
import           GHC.ByteOrder                           (ByteOrder (..))
import           PlutusLedgerApi.V1.Value                (lovelaceValue)
import           PlutusLedgerApi.V3                      as V3
import           PlutusTx                                (CompiledCode)
import           PlutusTx.Builtins                       (byteStringToInteger)
import           PlutusTx.Prelude                        ((<>))
import           Prelude                                 (Bool (..), Either (..), FilePath, IO, Integer, Maybe (..),
                                                          Show (..), String, concat, const, either, maybe, putStr, read,
                                                          return, ($), (++), (.), (>>))
import           System.Directory                        (createDirectoryIfMissing, getCurrentDirectory)
import           System.Environment                      (getArgs)
import           System.Exit                             (exitFailure)
import           System.FilePath                         (takeFileName, (</>))
import qualified System.IO                               as IO
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)
import           Text.Parsec                             (many1)
import           Text.Parsec.Char                        (digit)
import           Text.Parsec.String                      (Parser)
import           Text.Printf                             (printf)
import           Text.Read                               (readEither)

import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E             (IdentityCircuitContract (..), RollupInfo (..))
import           ZkFold.Cardano.OnChain.BLS12_381        (F (..), toF)
import           ZkFold.Cardano.OnChain.Utils            (dataToBlake)
import           ZkFold.Cardano.UPLC                     (parkingSpotCompiled, rollupCompiled, rollupDataCompiled)
import           ZkFold.Cardano.UPLC.Rollup              (RollupRedeemer (..), RollupSetup (..))
import           ZkFold.Cardano.UPLC.RollupData          (RollupDataRedeemer (..))


saveRollupPlutus :: FilePath -> TxOutRef -> V3.Address -> IO ()
saveRollupPlutus path oref addr = do
  x  <- generate arbitrary
  ps <- generate arbitrary

  let contract = IdentityCircuitContract x ps

  BL.writeFile (path </> "test-data" </> "plonk-raw-contract-data.json") $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n"

  let dataUpdate = dataToBlake [fromString "deadbeef" :: BuiltinByteString]
      update     = [dataUpdate]

  let (ledgerRules, iniState, _) = identityCircuitVerificationBytes x ps
      F iniState'                = iniState
      nextState                  = toF . byteStringToInteger BigEndian $ dataToBlake
                                   (iniState, update, [] :: [V3.TxOut], lovelaceValue $ Lovelace 15000000)
      (_, _, proof)              = stateCheckVerificationBytes x ps nextState

  let threadCS    = currencySymbolOf $ nftPolicyCompiled oref
      threadName  = TokenName (fromString "zkFold" :: BuiltinByteString)
      rollupSetup = RollupSetup
                    { rsLedgerRules  = ledgerRules
                    , rsDataCurrency = currencySymbolOf rollupDataCompiled
                    , rsThreadValue  = lovelaceValue (Lovelace 3000000) <> singleton threadCS threadName 1
                    , rsFeeAddress   = addr
                    }

  let rollupRedeemer     = UpdateRollup proof update
      rollupDataRedeemer = NewData [fromString "deadbeef" :: BuiltinByteString]

  let rollupInfoA = RollupInfo { riNextState = nextState, riRedeemer = rollupRedeemer }

  let assetsPath = path </> "assets"

  savePlutus (assetsPath </> "rollup.plutus") $ rollupCompiled rollupSetup
  savePlutus (assetsPath </> "rollupData.plutus") rollupDataCompiled

  BS.writeFile (assetsPath </> "unit.cbor") $ dataToCBOR ()
  BS.writeFile (assetsPath </> "datumB.cbor") $ dataToCBOR iniState'
  BS.writeFile (assetsPath </> "rollupInfoA.json") $ prettyPrintJSON $ dataToJSON rollupInfoA
  BS.writeFile (assetsPath </> "dataRedeemer.cbor") $ dataToCBOR rollupDataRedeemer
  IO.writeFile (assetsPath </> "dataTokenName.txt") . byteStringAsHex . fromBuiltin $ dataUpdate

saveParkingSpotPlutus :: FilePath -> IO ()
saveParkingSpotPlutus path = savePlutus (path </> "assets" </> "parkingSpot.plutus") (parkingSpotCompiled 43)

saveNftPolicyPlutus :: FilePath -> TxOutRef -> IO ()
saveNftPolicyPlutus path oref = savePlutus (path </> "assets" </> "nftPolicy.plutus") (nftPolicyCompiled oref)

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "rollup"   -> ".." </> ".."
        "e2e-test" -> ".."
        _          -> "."

  createDirectoryIfMissing True $ path </> "test-data"
  createDirectoryIfMissing True $ path </> "assets"

  argsRaw <- getArgs

  case argsRaw of
    (nftOrefStr : addrStr : _) -> do
      let argsE = do
            nftOref <- parseTxOutRef nftOrefStr
            addr    <- parseAddress addrStr
            return (nftOref, addr)
      case argsE of
        Right (nftOref, addr) -> do
          saveRollupPlutus path nftOref addr
          saveParkingSpotPlutus path
          saveNftPolicyPlutus path nftOref

          putStr "\nDone serializing plutus scripts and initializing state.\n\n"

        Left err -> putStr ("parse error: " ++ show err ++ "\n") >> exitFailure

    _ -> putStr "Error: please provide a pair of command-line string-arguments.\n" >> exitFailure


----- HELPER FUNCTIONS -----

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . toData

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . toData

-- | Write serialized script to a file.
writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

-- | Serialize plutus script
savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . serialiseCompiledCode

-- | Credential of compiled validator script
credentialOf :: CompiledCode a -> Credential
credentialOf = ScriptCredential . V3.ScriptHash . toBuiltin . serialiseToRawBytes . hashScript
               . PlutusScript plutusScriptVersion . PlutusScriptSerialised @PlutusScriptV3 . serialiseCompiledCode

-- | Currency symbol of compiled minting script
currencySymbolOf :: CompiledCode a -> CurrencySymbol
currencySymbolOf = CurrencySymbol . toBuiltin . serialiseToRawBytes . hashScript
                   . PlutusScript plutusScriptVersion . PlutusScriptSerialised @PlutusScriptV3 . serialiseCompiledCode

-- | Parser for a positive integer
integerParser :: Parser Integer
integerParser = do
  digits <- many1 digit
  return $ read digits

-- | Parse TxOutRef
parseTxOutRef :: String -> Either String TxOutRef
parseTxOutRef orefStr = do
  (txIdHex, txIxStr) <- case T.splitOn (T.pack "#") (T.pack orefStr) of
    [txIdHex, txIxStr] -> Right (txIdHex, txIxStr)
    _                  -> Left "Failed to parse TxOutRef"
  txId <- case deserialiseFromRawBytesHex AsTxId (TE.encodeUtf8 txIdHex) of
    Right txId' -> Right . V3.TxId . toBuiltin . serialiseToRawBytes $ txId'
    Left err    -> Left $ "Failed to parse TxId: " ++ show err
  txIx <- first (const "Failed to parse TxIx") (readEither (T.unpack txIxStr))
  return $ TxOutRef txId txIx

-- | Parse address in era
parseAddress :: String -> Either String V3.Address
parseAddress addressStr = do
    shellyAddr <- either (const $ Left "Failed to parse Shelly address") Right $
                  deserialiseFromBech32 (AsAddress AsShelleyAddr) (T.pack addressStr)
    pkh        <- maybe (Left "Failed to parse address pubkey hash") Right $
                  shelleyPayAddrToPlutusPubKHash shellyAddr
    return $ V3.Address (PubKeyCredential pkh) Nothing

-- | Get hex representation of bytestring
byteStringAsHex :: BS.ByteString -> String
byteStringAsHex bs = concat $ BS.foldr' (\w s -> (printf "%02x" w):s) [] bs
