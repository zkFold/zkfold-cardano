module Main where

import           Cardano.Api                                 (getScriptData, prettyPrintJSON, unsafeHashableScriptData)
import           Cardano.Api.Ledger                          (toCBOR)
import           Cardano.Api.Shelley                         (fromPlutusData, scriptDataFromJsonDetailedSchema,
                                                              scriptDataToJsonDetailedSchema, toPlutusData)
import           Codec.CBOR.Write                            (toStrictByteString)
import           Data.Aeson                                  (decode)
import qualified Data.Aeson                                  as Aeson
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BL
import           Data.Maybe                                  (fromJust)
import qualified PlutusLedgerApi.V3                          as V3
import           PlutusTx                                    (ToData (..))
import           Prelude                                     (Either (..), IO, concat, error, length, show, ($), (.),
                                                              (<$>), (==))
import           System.Directory                            (getCurrentDirectory)
import           System.FilePath                             (takeFileName, (</>))
import qualified System.IO                                   as IO

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret
import           ZkFold.Cardano.Examples.IdentityCircuit     (stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E                 (IdentityCircuitContract (..), RollupData (..))
import           ZkFold.Cardano.OnChain.BLS12_381            (toInput)
import           ZkFold.Cardano.OnChain.Utils                (dataToBlake)
import           ZkFold.Cardano.UPLC.Rollup                  (RollupRedeemer (..))

nextRollup :: Fr -> PlonkupProverSecret BLS12_381_G1 -> RollupData -> RollupData
nextRollup x ps rollupData =
  let nextState     = rdNextState rollupData
      nextUpdate    = concat [rrUpdate . rdRedeemer $ rollupData, [nextState]]
      nextState'    = toInput $ dataToBlake (nextState, nextUpdate)
      (_, _, proof) = stateCheckVerificationBytes x ps nextState'
      nextRedeemer = RollupRedeemer
                     { rrProof   = proof
                     , rrAddress = rrAddress . rdRedeemer $ rollupData
                     , rrValue   = rrValue . rdRedeemer $ rollupData
                     , rrState   = nextState
                     , rrUpdate  = nextUpdate
                     }
  in RollupData nextState' nextRedeemer

-- | Will process two simultaneous transactions 'A' & 'B', uploading states 'nextStateA', 'nextStateB'
-- with redeemers 'redeemerRollupA', 'redeemerRollupB', respectively.

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let currentDirName = takeFileName currentDir
      path           = if currentDirName == "rollup" then (".." </> "..")
                          else if currentDirName == "e2e-test" then ".." else "."
      assetsPath     = path </> "assets"

  rollupDataAE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assetsPath </> "rollupDataA.json")

  case rollupDataAE of
    Right rollupDataAScriptData -> do
      IdentityCircuitContract x ps <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "plonk-raw-contract-data.json")

      let rollupDataA = fromJust . V3.fromData . toPlutusData . getScriptData $ rollupDataAScriptData :: RollupData

      let RollupData nextStateA redeemerRollupA               = rollupDataA
      let rollupDataB@(RollupData nextStateB redeemerRollupB) = nextRollup x ps rollupDataA
      let newRollupDataA                                      = nextRollup x ps rollupDataB

      BS.writeFile (assetsPath </> "datumA.cbor") $ dataToCBOR nextStateA
      BS.writeFile (assetsPath </> "redeemerRollupA.cbor") $ dataToCBOR redeemerRollupA

      BS.writeFile (assetsPath </> "datumB.cbor") $ dataToCBOR nextStateB
      BS.writeFile (assetsPath </> "redeemerRollupB.cbor") $ dataToCBOR redeemerRollupB
      IO.writeFile (assetsPath </> "last-update-length.log") . show . length . rrUpdate $ redeemerRollupB

      BS.writeFile (assetsPath </> "newRollupDataA.json") $ prettyPrintJSON $ dataToJSON newRollupDataA

    Left _                      -> error "JSON error: unreadable 'rollupDataA.json'"


----- HELPER FUNCTIONS -----

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . V3.toData
