module Main where

import           Cardano.Api                           (IsPlutusScriptLanguage, PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Shelley                   (File (..), PlutusScript (..))
import           Control.Monad                         (void)
import qualified PlutusLedgerApi.V3                    as PlutusV3
import           PlutusTx                              (CompiledCode)
import           Prelude                               (FilePath, IO, Integer, Maybe (..), Show (..), print, ($), (++), (.))
import           Scripts                               (compiledPlonkVerifier, compiledforwardingMint)
import           Test.QuickCheck                       (variant)
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)

import           ZkFold.Cardano.Examples.EqualityCheck (equalityCheckVerificationBytes)

writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

main :: IO ()
main = do
  let seed = 5 :: Integer
  x           <- generate $ variant seed arbitrary
  ps          <- generate $ variant seed arbitrary
  targetValue <- generate $ variant seed arbitrary
  print $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue

  let (setup, _, _) = equalityCheckVerificationBytes x ps targetValue

  savePlutus ".././assets/assets/plonkVerifier.plutus" $ compiledPlonkVerifier setup
  savePlutus ".././assets/assets/forwardingMint.plutus" compiledforwardingMint
