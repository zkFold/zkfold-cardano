{-# OPTIONS_GHC -Wall -Wno-unused-matches #-}

module Main where

-- import           Backend.JsonToData                      (parseJsonToTxInInfoList)
import           Cardano.Api                             
import           Cardano.Api.Ledger                      (toCBOR)
import           Cardano.Api.Shelley                     (File (..), PlutusScript (..), fromPlutusData, toPlutusData,
                                                          scriptDataToJsonDetailedSchema)
import           Codec.CBOR.Write                        (toStrictByteString)
import           Control.Monad                           (void)
import           Data.Aeson                              (decode, eitherDecode)
import qualified Data.Aeson                              as Aeson
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
-- import           Data.Maybe                              (fromJust)
import           PlutusLedgerApi.V3                      as V3 hiding (TxOut)
import           PlutusTx                                (CompiledCode)
import           PlutusTx.Builtins.Internal              (serialiseData)
-- import           PlutusTx.Prelude                        (Ordering (..), blake2b_224, compare, sortBy)
-- import           PlutusTx.Prelude                        (blake2b_224, concat, drop, take)
-- import           Prelude                                 (Either (..), FilePath, IO, Maybe (..), Show (..), concat,
import           Prelude                                 (Either (..), FilePath, IO, Maybe (..), Show (..),
                                                          putStrLn, sequenceA, ($), (++), (.), (<$>), (>>))
import           System.Directory                        (getCurrentDirectory)
-- import           System.Exit                             (exitFailure)
import           System.FilePath                         (takeFileName, (</>))

-- import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes, stateCheckVerificationBytes)
-- import           ZkFold.Cardano.OffChain.E2E             (IdentityCircuitContract (..))
-- import           ZkFold.Cardano.OnChain.BLS12_381        (toInput)
import           ZkFold.Cardano.UPLC                     (parkingSpotCompiled, plonkVerifierTxCompiled')


main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "symbolic-balancing" -> ".." </> ".."
        "backends"           -> ".."
        "e2e-test"           -> ".."
        _                    -> "."

  let assetsPath = path </> "assets"

  -- txin3 <- parseJsonToTxInInfoList [Just $ parkingSpotCompiled 54] <$> BL.readFile (assetsPath </> "utxo3.json")
  content <- BL.readFile (assetsPath </> "utxo5.json")

  -- case eitherDecode content of
  --   Left err                               -> putStrLn $ "Error parsing JSON: " ++ err
  --   Right (txOut :: TxOut CtxTx ConwayEra) -> do
  --     putStrLn $ show txOut

  case eitherDecode content of
    Left err  -> putStrLn $ "Error parsing JSON: " ++ err
    Right obj -> case scriptDataFromJson ScriptDataJsonDetailedSchema obj of
      Left err -> putStrLn $ show err
      Right hsd -> do
        putStrLn "It worked!"
        putStrLn $ show hsd
        putStrLn $ show $ hashScriptDataBytes hsd

        let sd = getScriptData hsd
            pd = toPlutusData sd

        putStrLn $ show sd
        putStrLn $ show pd
        putStrLn $ show $ (unsafeFromData pd :: Datum)
        

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
