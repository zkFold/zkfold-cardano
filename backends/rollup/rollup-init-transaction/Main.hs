module Main where

import           Cardano.Api                             hiding (Lovelace)
import           Cardano.Api.Ledger                      (toCBOR)
import           Cardano.Api.Shelley                     (PlutusScript (..), fromPlutusData,
                                                          scriptDataToJsonDetailedSchema,
                                                          shelleyPayAddrToPlutusPubKHash)
import           Codec.CBOR.Write                        (toStrictByteString)
import           Control.Monad                           (mapM, void)
import           Data.Aeson                              (encode)
import qualified Data.Aeson                              as Aeson
import           Data.Bifunctor                          (first)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.String                             (IsString (fromString))
import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as TE
import           PlutusLedgerApi.V1.Value                (lovelaceValue)
import           PlutusLedgerApi.V3                      as V3
import           PlutusTx                                (CompiledCode)
import           PlutusTx.Prelude                        ((<>))
import           Prelude                                 (Bool (..), Either (..), FilePath, IO, Int, Integer,
                                                          Maybe (..), Show (..), String, concat, const, either, error,
                                                          length, map, maybe, putStr, read, return, ($), (++), (-), (.),
                                                          (<$>))
import           Rollup.Example                          (datumHashBSEx1, datumHashEx1)
import           System.Directory                        (createDirectoryIfMissing, getCurrentDirectory)
import           System.Environment                      (getArgs)
import           System.FilePath                         (takeFileName, (</>))
import qualified System.IO                               as IO
import           System.Random                           (randomRIO)
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)
import           Text.Parsec                             (many1)
import           Text.Parsec.Char                        (digit)
import           Text.Parsec.String                      (Parser)
import           Text.Printf                             (printf)
import           Text.Read                               (readEither)

import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E             (IdentityCircuitContract (..), RollupInfo (..))
import           ZkFold.Cardano.OnChain.BLS12_381        (F (..), bls12_381_field_prime, toInput)
import           ZkFold.Cardano.OnChain.Utils            (dataToBlake)
import           ZkFold.Cardano.UPLC                     (nftPolicyCompiled, parkingSpotCompiled, rollupCompiled,
                                                          rollupDataCompiled)
import           ZkFold.Cardano.UPLC.Rollup              (RollupRedeemer (..), RollupSetup (..))


updateLength :: Int
updateLength = 3

rmax :: Integer
rmax = 1000

rollupFee, threadLovelace, minReq :: Lovelace
rollupFee      = Lovelace 15000000
threadLovelace = Lovelace  3000000
minReq         = Lovelace   995610

saveParkingSpotPlutus :: FilePath -> Integer -> IO ()
saveParkingSpotPlutus path parkingTag = do
  savePlutus (path </> "assets" </> "parkingSpot.plutus") $ parkingSpotCompiled parkingTag
  IO.writeFile (path </> "assets" </> "parkingTag.txt") $ show parkingTag

saveNftPolicyPlutus :: FilePath -> TxOutRef -> IO ()
saveNftPolicyPlutus path oref = savePlutus (path </> "assets" </> "nftPolicy.plutus") $ nftPolicyCompiled oref

saveRollupPlutus :: FilePath -> Integer -> TxOutRef -> V3.Address -> IO ()
saveRollupPlutus path parkingTag oref addr = do
  x         <- generate arbitrary
  ps        <- generate arbitrary
  seeds     <- mapM (\_ -> randomRIO (1, rmax)) [1..updateLength]
  iniState' <- randomRIO (0, bls12_381_field_prime - 1)

  let contract = IdentityCircuitContract x ps

  BL.writeFile (path </> "test-data" </> "plonk-raw-contract-data.json") $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n"

  let (ledgerRules, _, _) = identityCircuitVerificationBytes x ps

      dataUpdate  = map (\s -> [dataToBlake s]) seeds
      update      = dataToBlake <$> dataUpdate
      iniState    = F iniState'

  let bridgeTxOut = V3.TxOut { txOutAddress         = Address (credentialOf $ parkingSpotCompiled parkingTag) Nothing
                             , txOutValue           = lovelaceValue minReq
                             , txOutDatum           = OutputDatumHash datumHashEx1
                             , txOutReferenceScript = Nothing
                             }

      nextState = toInput $ dataToBlake (iniState, update, [bridgeTxOut], lovelaceValue rollupFee)

      (_, _, proof) = stateCheckVerificationBytes x ps nextState

  let threadCS    = currencySymbolOf $ nftPolicyCompiled oref
      threadName  = TokenName (fromString "zkFold" :: BuiltinByteString)
      rollupSetup = RollupSetup
                    { rsLedgerRules  = ledgerRules
                    , rsDataCurrency = currencySymbolOf rollupDataCompiled
                    , rsThreadValue  = lovelaceValue threadLovelace <> singleton threadCS threadName 1
                    , rsFeeAddress   = addr
                    }

  let rollupRedeemer = UpdateRollup proof update
      rollupInfo     = RollupInfo { riDataUpdate = dataUpdate, riState = nextState, riRedeemer = rollupRedeemer }

  let assetsPath = path </> "assets"

  savePlutus (assetsPath </> "rollup.plutus") $ rollupCompiled rollupSetup
  savePlutus (assetsPath </> "rollupData.plutus") rollupDataCompiled

  BS.writeFile (assetsPath </> "unit.cbor") $ dataToCBOR ()
  BS.writeFile (assetsPath </> "datum.cbor") $ dataToCBOR iniState'
  BS.writeFile (assetsPath </> "rollupInfo.json") $ prettyPrintJSON $ dataToJSON rollupInfo

  IO.writeFile (assetsPath </> "dataTokensAmount.txt") . show . length $ update
  IO.writeFile (assetsPath </> "bridgeDatumHash.txt") . byteStringAsHex . fromBuiltin $ datumHashBSEx1

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
          parkingTag <- randomRIO (1, 10000)

          saveParkingSpotPlutus path parkingTag
          saveNftPolicyPlutus path nftOref
          saveRollupPlutus path parkingTag nftOref addr

          putStr "\nDone serializing plutus scripts and initializing state.\n\n"

        Left err -> error $ "parse error: " ++ show err

    _ -> error "Error: please provide a pair of command-line arguments.\n"


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

