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
import           Prelude                          (Either (..), IO, error, ($), (++), (.), (<$>))

import           ZkFold.Cardano.OnChain.BLS12_381 (F (..), toInput)
import           ZkFold.Cardano.OnChain.Utils     (dataToBlake)
import           ZkFold.Cardano.UPLC.Rollup       (RollupRedeemer (..))


nextRedeemerRollup :: F -> RollupRedeemer -> RollupRedeemer
nextRedeemerRollup nextState previousRollup = RollupRedeemer
  { rrProof   = rrProof previousRollup
  , rrAddress = rrAddress previousRollup
  , rrValue   = rrValue previousRollup
  , rrState   = nextState
  , rrUpdate  = rrUpdate previousRollup ++ [nextState]
  }

main :: IO ()
main = do
  redeemerRollupBE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile "../../assets/redeemerRollupB.json"
  case redeemerRollupBE of
    Right redeemerRollupBScriptData -> do
      let redeemerRollupB = fromJust . V3.fromData . toPlutusData . getScriptData $ redeemerRollupBScriptData :: RollupRedeemer

      let nextStateA          = toInput $ dataToBlake (rrState redeemerRollupB, rrUpdate redeemerRollupB)
          nextRedeemerRollupA = nextRedeemerRollup nextStateA redeemerRollupB

          nextStateB          = toInput $ dataToBlake (rrState nextRedeemerRollupA, rrUpdate nextRedeemerRollupA)
          nextRedeemerRollupB = nextRedeemerRollup nextStateB nextRedeemerRollupA

      BS.writeFile "../../assets/datumRollupA.cbor" $ dataToCBOR nextStateA
      BS.writeFile "../../assets/nextRedeemerRollupA.cbor" $ dataToCBOR nextRedeemerRollupA
      BS.writeFile "../../assets/nextRedeemerRollupA.json" $ prettyPrintJSON $ dataToJSON nextRedeemerRollupA

      BS.writeFile "../../assets/datumRollupB.cbor" $ dataToCBOR nextStateB
      BS.writeFile "../../assets/nextRedeemerRollupB.cbor" $ dataToCBOR nextRedeemerRollupB
      BS.writeFile "../../assets/nextRedeemerRollupB.json" $ prettyPrintJSON $ dataToJSON nextRedeemerRollupB

    Left _                         -> error "JSON error: unreadable 'redeemerRollup.json'"


----- HELPER FUNCTIONS -----

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . V3.toData
