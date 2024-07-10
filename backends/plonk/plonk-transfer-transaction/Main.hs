module Main where

import           Cardano.Api                           (prettyPrintJSON, unsafeHashableScriptData)
import           Cardano.Api.Shelley                   (fromPlutusData, scriptDataToJsonDetailedSchema)
import           Data.Aeson                            (decode)
import qualified Data.Aeson                            as Aeson
import           Data.ByteString                       as BS (writeFile)
import qualified Data.ByteString.Lazy                  as BL
import qualified PlutusLedgerApi.V3                    as V3
import           PlutusTx                              (ToData (..))
import           Prelude                               (IO, Maybe (..), String, print, ($), (.))

import           ZkFold.Cardano.Examples.EqualityCheck (equalityCheckVerificationBytes)
import           ZkFold.Cardano.Plonk.OffChain         (Contract (..), toContract)

dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

main :: IO ()
main = do
  jsonRowContract <- BL.readFile "test-data/raw-contract-data.json"
  case decode jsonRowContract of
    Just rowContract -> do
      let Contract{..} = toContract rowContract
          (_, input, _) = equalityCheckVerificationBytes x ps targetValue

      BS.writeFile ".././assets/datum.json" (prettyPrintJSON $ dataToJSON input)
    _ -> print ("Could not deserialize" :: String)
