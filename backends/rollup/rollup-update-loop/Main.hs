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
import           Prelude                          (Either (..), IO, error, length, show, ($), (++), (.), (<$>), (==))
import           System.Directory                 (getCurrentDirectory)
import           System.FilePath                  (takeFileName, (</>))
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

-- | Will process two simultaneous transactions 'A' & 'B', processing states
-- 'stateA', 'stateB' with redeemers 'redeemerA', 'redeemerB', respectively.

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let currentDirName = takeFileName currentDir
      path           = if currentDirName == "rollup" then "../.."
                          else if currentDirName == "e2e-test" then ".." else "."
      assetsPath     = path </> "assets"

  redeemerRollupAE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assetsPath </> "redeemerRollupA.json")
  case redeemerRollupAE of
    Right redeemerRollupAScriptData -> do
      let redeemerRollupA = fromJust . V3.fromData . toPlutusData . getScriptData $ redeemerRollupAScriptData :: RollupRedeemer

      let nextStateA      = toInput $ dataToBlake (rrState redeemerRollupA, rrUpdate redeemerRollupA)
          redeemerRollupB = nextRedeemer nextStateA redeemerRollupA

          nextStateB          = toInput $ dataToBlake (rrState redeemerRollupB, rrUpdate redeemerRollupB)
          nextRedeemerRollupA = nextRedeemer nextStateB redeemerRollupB

      BS.writeFile (assetsPath </> "datumRollupA.cbor") $ dataToCBOR nextStateA
      BS.writeFile (assetsPath </> "nextRedeemerRollupA.cbor") $ dataToCBOR nextRedeemerRollupA
      BS.writeFile (assetsPath </> "nextRedeemerRollupA.json") $ prettyPrintJSON $ dataToJSON nextRedeemerRollupA

      BS.writeFile (assetsPath </> "datumRollupB.cbor") $ dataToCBOR nextStateB
      BS.writeFile (assetsPath </> "redeemerRollupB.cbor") $ dataToCBOR redeemerRollupB
      IO.writeFile (assetsPath </> "last-update-length.log") . show . length . rrUpdate $ redeemerRollupB

    Left _                         -> error "JSON error: unreadable 'redeemerRollupA.json'"


----- HELPER FUNCTIONS -----

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . V3.toData
