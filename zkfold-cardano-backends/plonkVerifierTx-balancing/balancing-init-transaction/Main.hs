module Main where

import           Cardano.Api                             (IsPlutusScriptLanguage, writeFileTextEnvelope)
import           Cardano.Api.Shelley                     (File (..), PlutusScript (..))
import           Control.Monad                           (void)
import           Data.Aeson                              (encode)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.String                             (fromString)
import           PlutusLedgerApi.V3                      as V3
import           Prelude                                 (Bool (..), FilePath, IO, Maybe (..), Show (..), putStr, ($),
                                                          (++))
import           System.Directory                        (createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath                         (takeFileName, (</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (dataToCBOR, plutusDataToCBOR, savePlutus)
import           ZkFold.Cardano.UPLC.Common              (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.PlonkVerifierTx     (plonkVerifierTxCompiled)


writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

someDatum :: Data
someDatum = Constr 0 [B $ fromString "deadbeef"]

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "plonkVerifierTx-balancing" -> ".." </> ".."
        "e2e-test"                  -> ".."
        _                           -> "."

  createDirectoryIfMissing True $ path </> "test-data"
  createDirectoryIfMissing True $ path </> "assets"

  x           <- generate arbitrary
  ps          <- generate arbitrary

  let contract = IdentityCircuitContract x ps

  BL.writeFile (path </> "test-data" </> "symbolic-contract-data.json") $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n\n"

  let (setup, _, _) = identityCircuitVerificationBytes x ps

  let assetsPath = path </> "assets"

  savePlutus (assetsPath </> "plonkVerifierTx.plutus") $ plonkVerifierTxCompiled setup
  savePlutus (assetsPath </> "parkingSpot.plutus") $ parkingSpotCompiled 54

  BS.writeFile (assetsPath </> "unit.cbor") $ dataToCBOR ()
  BS.writeFile (assetsPath </> "someDatum.cbor") $ plutusDataToCBOR someDatum
