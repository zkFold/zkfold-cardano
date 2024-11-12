module Main where

import           Cardano.Api                              (IsPlutusScriptLanguage, PlutusScriptV3,
                                                           writeFileTextEnvelope)
import           Cardano.Api.Ledger                       (toCBOR)
import           Cardano.Api.Shelley                      (File (..), PlutusScript (..), fromPlutusData, scriptDataFromJsonDetailedSchema)
import           Codec.CBOR.Write                         (toStrictByteString)
import           Control.Monad                            (void)
import           Data.Aeson                               (decode)
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Lazy                     as BL
import qualified PlutusLedgerApi.V3                       as V3
import           Data.Maybe                               (fromJust)
import           PlutusTx                                 (CompiledCode, ToData (..))
import           Prelude                                  (FilePath, IO, Maybe (..), Show (..), putStr, ($), (<$>), (++), (.))
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)

import           Bench.JsonToData                         (parseJsonToTxInInfoList)  -- displayTxIn
import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.Examples.IdentityCircuit  (stateCheckVerificationBytes)
import           ZkFold.Cardano.OnChain.BLS12_381         (F (..))
import           ZkFold.Cardano.OnChain.Plonk             (PlonkPlutus)
-- import           ZkFold.Cardano.UPLC                      (symbolicVerifierCompiled)


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
  x           <- generate arbitrary
  ps          <- generate arbitrary

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n\n"

  let input                = F 26217937587563095239723870254092982918845276250263818911301829349969290592256  -- an arbitrary value
  let (setup, _, proof) = stateCheckVerificationBytes x ps input

  let result = show $ verify @PlonkPlutus @HaskellCore setup input proof
  putStr $ "Result: " ++ result ++ ".\n\n"

  datumExampleE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile "datumExample.json"

  putStr $ (show datumExampleE) ++ "\n\n"

  txin <- parseJsonToTxInInfoList <$> BL.readFile "utxoExample.json"

  putStr $ (show txin) ++ "\n\n"

  -- txin' <- displayTxIn <$> BL.readFile "utxoExample.json"

  -- putStr $ (show txin') ++ "\n\n"
