module Main where

import           Cardano.Api                           (IsPlutusScriptLanguage, PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Shelley                   (File (..), PlutusScript (..))
import           Control.Monad                         (void)
import           Data.Aeson                            (encode)
import qualified Data.ByteString.Lazy                  as BL
import qualified PlutusLedgerApi.V3                    as PlutusV3
import           PlutusTx                              (CompiledCode)
import           Prelude                               (Bool (..), FilePath, IO, Maybe (..), Show (..), putStr, ($),
                                                        (++), (.))
import           System.Directory                      (createDirectoryIfMissing)
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)

import           ZkFold.Cardano.Examples.EqualityCheck (equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E           (EqualityCheckContract (..))
import           ZkFold.Cardano.UPLC                   (forwardingMintCompiled, plonkVerifierTokenCompiled)

writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

main :: IO ()
main = do
  x           <- generate arbitrary
  ps          <- generate arbitrary
  targetValue <- generate arbitrary

  let contract = EqualityCheckContract x ps targetValue

  createDirectoryIfMissing True "../test-data"
  createDirectoryIfMissing True "../assets"

  BL.writeFile "../test-data/plonk-raw-contract-data.json" $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n"

  let (setup, _, _) = equalityCheckVerificationBytes x ps targetValue

  let fmLabel = 0  -- Use a different label (number) to get another 'forwardingMint' address

  savePlutus "../assets/plonkVerifierToken.plutus" $ plonkVerifierTokenCompiled setup
  savePlutus "../assets/forwardingMint.plutus" $ forwardingMintCompiled fmLabel
