module Main where

import           Cardano.Api         (policyId, prettyPrintJSON, unsafeHashableScriptData)
import           Cardano.Api.Shelley (fromPlutusData, scriptDataToJsonDetailedSchema)
import qualified Data.Aeson          as Aeson
import           Data.ByteString     as BS (writeFile)
import qualified PlutusLedgerApi.V3  as V3
import           PlutusTx            (ToData (..))
import           Prelude             (Either (..), IO, Show (..), head, print, ($), (++), (.), (<$>))
import           System.Environment  (getArgs)
import           Text.Parsec         (parse)
import Data.Aeson (encode)
import Cardano.Api.Ledger (serialize', toCBOR)
import Cardano.Api (SerialiseAsCBOR(..))
import Cardano.Api (SerialiseAsRawBytes(..))
import Codec.CBOR.Write (toStrictByteString)

dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

main :: IO ()
main = do
  policyidE <- parse policyId "" . head <$> getArgs

  case policyidE of
    Right policyid -> BS.writeFile "../.././assets/datumPlonk.cbor" (toStrictByteString $ toCBOR $ serialiseToRawBytes policyid)
    Left err       -> print $ "parse" ++ show err
