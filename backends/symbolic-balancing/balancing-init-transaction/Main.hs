module Main where

import           Cardano.Api                             (IsPlutusScriptLanguage, PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Ledger                      (toCBOR)
import           Cardano.Api.Shelley                     (File (..), PlutusScript (..), fromPlutusData)
import           Codec.CBOR.Write                        (toStrictByteString)
import           Control.Monad                           (void)
import           Data.Aeson                              (encode)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import qualified PlutusLedgerApi.V3                      as V3
import           PlutusTx                                (CompiledCode, ToData (..))
import           Prelude                                 (Bool (..), FilePath, IO, Maybe (..), Show (..), putStr, ($),
                                                          (++), (.))
import           System.Directory                        (createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath                         (takeFileName, (</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E             (IdentityCircuitContract (..))
import           ZkFold.Cardano.UPLC                     (parkingSpotCompiled, plonkVerifierTxCompiled')


writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . V3.serialiseCompiledCode

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . V3.toData

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

  -- let input         = F 26217937587563095239723870254092982918845276250263818911301829349969290592256  -- an arbitrary value
  let (setup, _, _) = identityCircuitVerificationBytes x ps

  let assetsPath = path </> "assets"

  savePlutus (assetsPath </> "symbolic.plutus") $ plonkVerifierTxCompiled' setup
  savePlutus (assetsPath </> "parkingSpot.plutus") $ parkingSpotCompiled 54

  BS.writeFile (assetsPath </> "unit.cbor") $ dataToCBOR ()
