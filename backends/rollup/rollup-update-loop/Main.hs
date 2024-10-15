module Main where

import           Cardano.Api                      (getScriptData, prettyPrintJSON, unsafeHashableScriptData)
import           Cardano.Api.Ledger               (toCBOR)
import           Cardano.Api.Shelley              (fromPlutusData, scriptDataFromJsonDetailedSchema,
                                                   scriptDataToJsonDetailedSchema, toPlutusData)
import           Codec.CBOR.Write                 (toStrictByteString)
import           Data.Aeson                       (decode)
import qualified Data.Aeson                       as Aeson
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL
import           Data.Maybe                       (fromJust)
import qualified PlutusLedgerApi.V3               as V3
import           PlutusTx                         (ToData (..))
import           Prelude                          (Either (..), IO, error, length, show, ($), (++), (.), (<$>))
import qualified System.IO                        as IO

import           ZkFold.Cardano.OnChain.BLS12_381 (F (..), toInput)
import           ZkFold.Cardano.OnChain.Utils     (dataToBlake)
import           ZkFold.Cardano.UPLC.Rollup       (RollupRedeemer (..))


nextRedeemer :: F -> RollupRedeemer -> RollupRedeemer
nextRedeemer nextState previousRollup = RollupRedeemer
  { rrProof   = rrProof previousRollup
  , rrAddress = rrAddress previousRollup
  , rrValue   = rrValue previousRollup
  , rrState   = nextState
  , rrUpdate  = rrUpdate previousRollup ++ [nextState]
  }

main :: IO ()
main = do
  redeemerRollupE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile "../../assets/redeemerRollup.json"
  case redeemerRollupE of
    Right redeemerRollupScriptData -> do
      let redeemerRollup = fromJust . V3.fromData . toPlutusData . getScriptData $ redeemerRollupScriptData :: RollupRedeemer

      let nextState          = toInput $ dataToBlake (rrState redeemerRollup, rrUpdate redeemerRollup)
          nextRedeemerRollup = nextRedeemer nextState redeemerRollup

      IO.writeFile "../../assets/last-update-length.log" . show . length . rrUpdate $ redeemerRollup
      BS.writeFile "../../assets/datumRollup.cbor" $ dataToCBOR nextState
      BS.writeFile "../../assets/nextRedeemerRollup.cbor" $ dataToCBOR nextRedeemerRollup
      BS.writeFile "../../assets/nextRedeemerRollup.json" $ prettyPrintJSON $ dataToJSON nextRedeemerRollup

    Left _                         -> error "JSON error: unreadable 'redeemerRollup.json'"


----- HELPER FUNCTIONS -----

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . V3.toData
