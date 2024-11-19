module Main where

import           System.Directory                            (createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath                             (takeFileName, (</>))
import           ZkFold.Cardano.OffChain.E2E             (IdentityCircuitContract(..))
import           Data.Aeson                              (encode)



-- import           Bench.JsonToData                         (parseJsonToTxInInfoList)
import           Cardano.Api                              (IsPlutusScriptLanguage, PlutusScriptV3,
                                                           writeFileTextEnvelope)
import           Cardano.Api.Ledger                       (toCBOR)
import           Cardano.Api.Shelley                      (File(..), PlutusScript(..), fromPlutusData,
                                                           scriptDataFromJsonDetailedSchema)
import           Codec.CBOR.Write                         (toStrictByteString)
import           Control.Monad                            (void)
import           Data.Aeson                               (decode)
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Lazy                     as BL
import           Data.Maybe                               (fromJust)
import qualified PlutusLedgerApi.V3                       as V3
import           PlutusTx                                 (CompiledCode, ToData(..))
import           PlutusTx.Builtins.Internal               (serialiseData)
import           PlutusTx.Prelude                         (blake2b_224, head)
import           Prelude                                  (Bool(..), FilePath, Either(..), IO, Maybe(..), Show(..), putStr,
                                                           ($), (++), (.), (<$>))
import           Test.QuickCheck.Arbitrary                (Arbitrary(..))
import           Test.QuickCheck.Gen                      (generate)

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof(..))
import           ZkFold.Cardano.Examples.IdentityCircuit  (stateCheckVerificationBytes)
import           ZkFold.Cardano.OnChain.BLS12_381         (F(..), toInput)
import           ZkFold.Cardano.OnChain.Plonk             (PlonkPlutus)
import           ZkFold.Cardano.UPLC                      (symbolicVerifierCompiled', parkingSpotCompiled)


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

  BL.writeFile (path </> "test-data" </> "plonk-raw-contract-data.json") $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n\n"

  let input             = F 26217937587563095239723870254092982918845276250263818911301829349969290592256  -- an arbitrary value
  let (setup, _, proof) = stateCheckVerificationBytes x ps input

  let assetsPath = path </> "assets"

  savePlutus (assetsPath </> "symbolic.plutus") $ symbolicVerifierCompiled' setup

  savePlutus (assetsPath </> "parkingSpot.plutus") $ parkingSpotCompiled 54

  BS.writeFile (assetsPath </> "unit.cbor") $ dataToCBOR ()

{-

  let result = show $ verify @PlonkPlutus @HaskellCore setup input proof
  putStr $ "Result: " ++ result ++ ".\n\n"

  datumExampleE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile "datumExample.json"

  putStr $ (show datumExampleE) ++ "\n\n"

  txin <- parseJsonToTxInInfoList <$> BL.readFile "utxoExample.json"

  putStr $ (show txin) ++ "\n\n"

  case txin of
    Right txins -> do
      let txinBD  = toBuiltinData . head $ txins
      putStr $ "Data: " ++ (show txinBD) ++ "\n\n"

      let txinBBS = serialiseData txinBD
      putStr $ "Serialised data: " ++ (show txinBBS) ++ "\n\n"
  
      let input' = toInput $ blake2b_224 txinBBS
      putStr $ "Input: " ++ (show input') ++ "\n\n"

    Left errMsg -> putStr $ "Error: " ++ errMsg ++ "\n\n"
-}
