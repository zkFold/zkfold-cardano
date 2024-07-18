module Main where

import           Cardano.Api                           (AssetName (..), UsingRawBytesHex (..), prettyPrintJSON, unsafeHashableScriptData)
import           Cardano.Api.Shelley                   (fromPlutusData, scriptDataToJsonDetailedSchema)
import           Data.Aeson                            (decode)
import qualified Data.Aeson                            as Aeson
import           Data.ByteString                       as BS (writeFile)
import qualified Data.ByteString.Lazy                  as BL
import           Data.Maybe                            (fromJust)
import           Data.String                           (IsString (..))
import           PlutusLedgerApi.V3                    (fromBuiltin)
import qualified PlutusLedgerApi.V3                    as V3
import           PlutusTx                              (ToData (..))
import           Prelude                               (IO, putStr, show, ($), (++), (.), (<$>))

import           ZkFold.Cardano.Examples.EqualityCheck (equalityCheckVerificationBytes)
import           ZkFold.Cardano.Plonk.OffChain         (Contract (..))
import           ZkFold.Cardano.Plonk.OnChain.Utils    (fromInput)

dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

main :: IO ()
main = do
  Contract{..} <- fromJust . decode <$> BL.readFile "../../test-data/plonk-raw-contract-data.json"

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n"

  let (_, input, proof) = equalityCheckVerificationBytes x ps targetValue

  BS.writeFile "../../assets/tokenname" $ fromString $ show $ UsingRawBytesHex $ AssetName $ fromBuiltin $ fromInput input
  BS.writeFile "../../assets/unit.json" $ prettyPrintJSON $ dataToJSON ()
  BS.writeFile "../../assets/redeemerPlonkVerifier.json" $ prettyPrintJSON $ dataToJSON proof
