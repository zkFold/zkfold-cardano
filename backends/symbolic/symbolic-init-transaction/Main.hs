module Main where

import           Cardano.Api                           (IsPlutusScriptLanguage, PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Shelley                   (File (..), PlutusScript (..))
import           Control.Monad                         (void)
import           Data.Aeson                            (decode)
import qualified Data.ByteString.Lazy                  as BL
import           Data.Maybe                            (fromJust)
import qualified PlutusLedgerApi.V3                    as PlutusV3
import           PlutusTx                              (CompiledCode)
import           Prelude                               (FilePath, IO, Maybe (..), ($), (.), (<$>))
import           Scripts                               (compiledForwardingReward, compiledSymbolicVerifier)

import           ZkFold.Cardano.Examples.EqualityCheck (equalityCheckVerificationBytes)
import           ZkFold.Cardano.Plonk.OffChain         (Contract (..), toContract)

writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

main :: IO ()
main = do
  Contract{..} <- toContract . fromJust . decode <$> BL.readFile "test-data/raw-contract-data.json"

  let (setup, _, _) = equalityCheckVerificationBytes x ps targetValue

  savePlutus ".././assets/symbolicVerifier.plutus" $ compiledSymbolicVerifier setup
  savePlutus ".././assets/forwardingReward.plutus" compiledForwardingReward
