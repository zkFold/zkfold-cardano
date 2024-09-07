module Main where

import           Cardano.Api                              (IsPlutusScriptLanguage, PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Ledger                       (toCBOR)
import           Cardano.Api.Shelley                      (File (..), PlutusScript (..), fromPlutusData)
import           Control.Monad                            (void)
import           Codec.CBOR.Write                         (toStrictByteString)
import           Data.Aeson                               (encode)
import qualified Data.ByteString.Lazy                     as BL
import qualified Data.ByteString                          as BS
import qualified PlutusLedgerApi.V3                       as V3
import           PlutusTx                                 (CompiledCode, ToData (..))
import           Prelude                                  (Bool (..), FilePath, IO, Int, Integer, Maybe (..), Show (..), putStr, 
                                                           (-), ($), (++), (.))
import           System.Directory                         (createDirectoryIfMissing)
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)

import           Bench.Scripts                            (compiledPubInput, compiledSymbolicVerifierBench1)
import           Bench.Utils                              (memToBS)
import           ZkFold.Cardano.Examples.EqualityCheck    (equalityCheckVerificationBytes)
import           ZkFold.Cardano.Plonk.OffChain            (EqualityCheckContract (..))
import           ZkFold.Cardano.Plonk.OnChain.Data        (InputBytes, ProofBytes(..))
import           ZkFold.Cardano.Plonk.OnChain.Utils       (dataToBlake, toInput)
import qualified ZkFold.Cardano.Plonk.OnChain.BLS12_381.F as F


writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . V3.serialiseCompiledCode

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . V3.toData

dataSize :: Int
dataSize = 1

inputData :: V3.BuiltinData
inputData = V3.toBuiltinData . V3.toBuiltin . memToBS $ dataSize

genericRedeemer :: ProofBytes
genericRedeemer = ProofBytes a a a a a a a b b n n n n n n (F.F m)
  where a = V3.toBuiltin $ memToBS 48   -- element on G1
        b = V3.toBuiltin $ memToBS 96   -- element on G2
        n = 0xffffffffffffffffffffffff  -- 32-byte number
        m = F.bls12_381_field_prime - 1

inputBytes :: InputBytes
inputBytes = toInput $ dataToBlake (0 :: Integer)

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

  let (setup, _, _) = equalityCheckVerificationBytes x ps targetValue

  savePlutus "../../assets/pubInput.plutus" compiledPubInput
  savePlutus "../../assets/compiledSymbolicVerifierBench1.plutus" $ compiledSymbolicVerifierBench1 setup inputBytes

  -- BS.writeFile "../../assets/unit.cbor" $ dataToCBOR ()
  BS.writeFile "../../assets/input-data.cbor" $ dataToCBOR inputData
  BS.writeFile "../../assets/genericRedeemer" $ dataToCBOR genericRedeemer
