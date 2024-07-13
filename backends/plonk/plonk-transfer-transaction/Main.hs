module Main where

import           Cardano.Api         (prettyPrintJSON, unsafeHashableScriptData)
import           Cardano.Api.Shelley (fromPlutusData, scriptDataToJsonDetailedSchema)
import qualified Data.Aeson          as Aeson
import           Data.ByteString     as BS (writeFile)
import qualified PlutusLedgerApi.V3  as V3
import           PlutusTx            (ToData (..))
import           PlutusTx.Builtins   (BuiltinByteString)
import           Prelude             (IO, String, head, undefined, ($), (.), (<$>))
import           System.Environment  (getArgs)

dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

magic :: String -> BuiltinByteString
magic = undefined

main :: IO ()
main = do
  policyid <- head <$> getArgs

  BS.writeFile ".././assets/datumPlonk.json" (prettyPrintJSON $ dataToJSON $ magic policyid)
