module Main where

import           Cardano.Api                             (IsPlutusScriptLanguage, PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Ledger                      (toCBOR)
import           Cardano.Api.Shelley                     (File (..), PlutusScript (..), fromPlutusData)
import           Codec.CBOR.Write                        (toStrictByteString)
import           Control.Monad                           (void)
import           Data.Aeson                              (encode)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.String                             (fromString)
import           PlutusLedgerApi.V3                      as V3
import           PlutusTx                                (CompiledCode)
import           Prelude                                 (Bool (..), FilePath, IO, Maybe (..), Show (..), putStr, ($),
                                                          (++), (.))
import           System.Directory                        (createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath                         (takeFileName, (</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E             (IdentityCircuitContract (..))
import           ZkFold.Cardano.UPLC                     (parkingSpotCompiled, plonkVerifierTxCompiled)


writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . V3.serialiseCompiledCode

someDatum :: Data
someDatum = Constr 0 [B $ fromString "deadbeef"]

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "symbolic-balancing" -> ".." </> ".."
        "backends"           -> ".."
        "e2e-test"           -> ".."
        _                    -> "."

  createDirectoryIfMissing True $ path </> "test-data"
  createDirectoryIfMissing True $ path </> "assets"

  x           <- generate arbitrary
  ps          <- generate arbitrary

  let contract = IdentityCircuitContract x ps

  BL.writeFile (path </> "test-data" </> "symbolic-contract-data.json") $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n\n"

  let (setup, _, _) = identityCircuitVerificationBytes x ps

  let assetsPath = path </> "assets"

  savePlutus (assetsPath </> "symbolic.plutus") $ plonkVerifierTxCompiled setup
  savePlutus (assetsPath </> "parkingSpot.plutus") $ parkingSpotCompiled 54

  BS.writeFile (assetsPath </> "unit.cbor") $ dataToCBOR ()
  BS.writeFile (assetsPath </> "someDatum.cbor") $ plutusDataToCBOR someDatum


----- HELPER FUNCTIONS -----

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . V3.toData

-- | Serialise Plutus data to CBOR.
plutusDataToCBOR :: Data -> BS.ByteString
plutusDataToCBOR = toStrictByteString . toCBOR . fromPlutusData
