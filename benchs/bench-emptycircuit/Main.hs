module Main where

import           Cardano.Api                              (IsPlutusScriptLanguage, PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Ledger                       (toCBOR)
import           Cardano.Api.Shelley                      (File (..), PlutusScript (..), fromPlutusData)
import           Control.Monad                            (void)
import           Codec.CBOR.Write                         (toStrictByteString)
import           Data.Aeson                               (encode)  -- decode
import qualified Data.ByteString.Lazy                     as BL
import qualified Data.ByteString                          as BS
-- import           Data.Maybe                               (fromJust)
import qualified PlutusLedgerApi.V3                       as V3
import           PlutusTx                                 (CompiledCode, ToData (..))
-- import           PlutusTx.Builtins                        (bls12_381_G1_compressed_generator, bls12_381_G1_compress,
--                                                            bls12_381_G1_scalarMul, bls12_381_G1_uncompress)
import           Prelude                                  (Bool (..), FilePath, IO, Maybe (..), Show (..), putStr, ($), (++), (.))  -- (<$>)
import           System.Directory                         (createDirectoryIfMissing)
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)

import           Bench.Scripts                            (compiledAlwaysSucceeds)
-- import           ZkFold.Cardano.Examples.EqualityCheck    (equalityCheckVerificationBytes)
import           ZkFold.Cardano.Benchs.EmptyCircuit       (tautologyVerificationBytes)
import           ZkFold.Cardano.Plonk.OffChain            (EqualityCheckContract (..))
import           ZkFold.Cardano.UPLC                      (symbolicVerifierCompiled)
-- import           ZkFold.Cardano.Plonk.OnChain.Data        (ProofBytes(..))
-- import qualified ZkFold.Cardano.Plonk.OnChain.BLS12_381.F as F


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

  let contract = EqualityCheckContract x ps targetValue

  createDirectoryIfMissing True "../../test-data"
  createDirectoryIfMissing True "../../assets"

  BL.writeFile "../../test-data/symbolic-raw-contract-data.json" $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n"

  let (setup, _, proof) = tautologyVerificationBytes x ps targetValue

  savePlutus "../../assets/alwaysSucceeds.plutus" $ compiledAlwaysSucceeds 1845
  savePlutus "../../assets/symbolicVerifier.plutus" $ symbolicVerifierCompiled setup

  BS.writeFile "../../assets/unit.cbor" $ dataToCBOR ()
  BS.writeFile "../../assets/redeemerSymbolicVerifier.cbor" $ dataToCBOR proof


{-
main :: IO ()
main = do
  EqualityCheckContract{..} <- fromJust . decode <$> BL.readFile "../../test-data/plonk-raw-contract-data.json"

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n"

  let (setup, _, proof) = tautologyVerificationBytes x ps targetValue

  savePlutus "../../assets/symbolicVerifier.plutus" $ compiledSymbolicVerifier setup
  BS.writeFile "../../assets/redeemerSymbolicVerifier.cbor" $ dataToCBOR proof
-}
