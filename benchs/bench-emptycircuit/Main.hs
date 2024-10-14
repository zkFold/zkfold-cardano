module Main where

-- import           Bench.Scripts                      (compiledAlwaysSucceeds)
import           Cardano.Api                        (IsPlutusScriptLanguage, PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Ledger                 (toCBOR)
import           Cardano.Api.Shelley                (File (..), PlutusScript (..), fromPlutusData)
import           Codec.CBOR.Write                   (toStrictByteString)
import           Control.Monad                      (void)
-- import           Data.Aeson                         (encode)
import qualified Data.ByteString                    as BS
-- import qualified Data.ByteString.Lazy               as BL
import qualified PlutusLedgerApi.V3                 as V3
import           PlutusTx                           (CompiledCode, ToData (..))
import           Prelude                            (FilePath, IO, Maybe (..), Show (..), putStr, ($), (++), (.))  -- Bool (..)
-- import           System.Directory                   (createDirectoryIfMissing)
import           Test.QuickCheck.Arbitrary          (Arbitrary (..))
import           Test.QuickCheck.Gen                (generate)

import           ZkFold.Base.Protocol.NonInteractiveProof    (HaskellCore, NonInteractiveProof (..))
-- import           ZkFold.Cardano.Examples.EqualityCheck (equalityCheckVerificationBytes)
import           ZkFold.Cardano.Benchs.EmptyCircuit (tautologyVerificationBytes)
-- import           ZkFold.Cardano.OffChain.E2E        (EqualityCheckContract (..))
import           ZkFold.Cardano.OnChain.Plonk       (PlonkPlutus)
-- import           ZkFold.Cardano.UPLC                (symbolicVerifierCompiled)


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
  targetValue <- generate arbitrary

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n\n"

  -- let (setup, input, proof) = equalityCheckVerificationBytes x ps targetValue
  let (setup, input, proof) = tautologyVerificationBytes x ps targetValue

  let result = show $ verify @PlonkPlutus @HaskellCore setup input proof
  putStr $ "Result: " ++ result ++ ".\n"


{-
main :: IO ()
main = do
  x           <- generate arbitrary
  ps          <- generate arbitrary
  targetValue <- generate arbitrary

  let contract = EqualityCheckContract x ps targetValue

  createDirectoryIfMissing True "../../test-data"
  createDirectoryIfMissing True "../../assets"

  BL.writeFile "../../test-data/symbolic-raw-contract-data.json" $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n\n"

  let (setup, _, proof) = tautologyVerificationBytes x ps targetValue

  savePlutus "../../assets/alwaysSucceeds.plutus" $ compiledAlwaysSucceeds 17
  savePlutus "../../assets/symbolicVerifier.plutus" $ symbolicVerifierCompiled setup

  BS.writeFile "../../assets/unit.cbor" $ dataToCBOR ()
  BS.writeFile "../../assets/redeemerSymbolicVerifier.cbor" $ dataToCBOR proof
-}
