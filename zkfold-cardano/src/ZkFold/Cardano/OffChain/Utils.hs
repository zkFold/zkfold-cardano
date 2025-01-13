module ZkFold.Cardano.OffChain.Utils where

import           Cardano.Api         hiding (Lovelace)
import           Cardano.Api.Ledger  (toCBOR)
import           Cardano.Api.Shelley (PlutusScript (..), fromPlutusData, scriptDataToJsonDetailedSchema)
import           Codec.CBOR.Write    (toStrictByteString)
import           Control.Monad       (void)
import qualified Data.Aeson          as Aeson
import qualified Data.ByteString     as BS
import           PlutusLedgerApi.V3  (ToData, serialiseCompiledCode, toData)
import           PlutusTx            (CompiledCode)
import           PlutusTx.Prelude    ((.))
import           Prelude             hiding (Bool, Eq (..), Fractional (..), Num (..), length, (.))

-- | Write serialized script to a file.
writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

-- | Serialize plutus script.
savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . serialiseCompiledCode

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . toData

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . toData
