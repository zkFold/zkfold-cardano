module Main where

import           Cardano.Api         (SerialiseAsRawBytes (..), policyId, unsafeHashableScriptData)
import           Cardano.Api.Ledger  (toCBOR)
import           Cardano.Api.Shelley (fromPlutusData, scriptDataToJsonDetailedSchema)
import           Codec.CBOR.Write    (toStrictByteString)
import qualified Data.Aeson          as Aeson
import           Data.ByteString     as BS (writeFile)
import qualified PlutusLedgerApi.V3  as V3
import           PlutusTx            (ToData (..))
import           Prelude             (Either (..), IO, Show (..), head, print, ($), (++), (.), (<$>))
import           System.Environment  (getArgs)
import           Text.Parsec         (parse)

dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

main :: IO ()
main = do
  policyidE <- parse policyId "" . head <$> getArgs

  case policyidE of
    Right policyid -> BS.writeFile "../../assets/datumSymbolic.cbor" $ toStrictByteString $ toCBOR $ serialiseToRawBytes policyid
    Left err       -> print $ "parse" ++ show err
