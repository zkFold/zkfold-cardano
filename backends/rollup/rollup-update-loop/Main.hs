module Main where

import           Cardano.Api          (getScriptData, prettyPrintJSON, unsafeHashableScriptData)
import           Cardano.Api.Ledger   (toCBOR)
import           Cardano.Api.Shelley  (fromPlutusData, scriptDataFromJsonDetailedSchema,
                                       scriptDataToJsonDetailedSchema, toPlutusData)
import           Codec.CBOR.Write     (toStrictByteString)
import           Data.Aeson           (decode)
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe           (fromJust)
import qualified PlutusLedgerApi.V3   as V3
import           PlutusTx             (ToData (..))
import           Prelude              (Either (..), IO, error, length, show, ($), (++), (.), (<$>), (==), putStr)
import           System.Directory     (getCurrentDirectory)
import           System.FilePath      (takeFileName, (</>))
import qualified System.IO            as IO

-- import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
-- import           ZkFold.Cardano.OnChain.Plonk             (PlonkPlutus)
import           ZkFold.Cardano.UPLC.Rollup               (rollupMini)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret
import           ZkFold.Cardano.Examples.IdentityCircuit     (identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E                 (IdentityCircuitContract (..))
import           ZkFold.Cardano.OnChain.BLS12_381            (F (..), toInput)
import           ZkFold.Cardano.OnChain.Utils                (dataToBlake)
import           ZkFold.Cardano.UPLC.Rollup                  (RollupRedeemer (..))

nextRollup :: Fr -> PlonkupProverSecret BLS12_381_G1 -> RollupRedeemer -> (F, RollupRedeemer)
nextRollup x ps previousRollup =
  let nextState     = toInput $ dataToBlake (rrState previousRollup, rrUpdate previousRollup)
      nextState'    = toInput $ dataToBlake (nextState, rrUpdate previousRollup ++ [nextState])
      (_, _, proof) = stateCheckVerificationBytes x ps nextState'
      nextRedeemer = RollupRedeemer
                     { rrProof   = proof
                     , rrAddress = rrAddress previousRollup
                     , rrValue   = rrValue previousRollup
                     , rrState   = nextState
                     , rrUpdate  = rrUpdate previousRollup ++ [nextState]
                     }
  in (nextState, nextRedeemer)

-- nextRedeemer :: F -> RollupRedeemer -> RollupRedeemer
-- nextRedeemer nextState previousRollup = RollupRedeemer
--   { rrProof   = rrProof previousRollup
--   , rrAddress = rrAddress previousRollup
--   , rrValue   = rrValue previousRollup
--   , rrState   = nextState
--   , rrUpdate  = rrUpdate previousRollup ++ [nextState]
--   }

-- | Will process two simultaneous transactions 'A' & 'B', processing states
-- 'stateA', 'stateB' with redeemers 'redeemerA', 'redeemerB', respectively.

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let currentDirName = takeFileName currentDir
      path           = if currentDirName == "rollup" then (".." </> "..")
                          else if currentDirName == "e2e-test" then ".." else "."
      assetsPath     = path </> "assets"

  redeemerRollupAE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assetsPath </> "redeemerRollupA.json")

  case redeemerRollupAE of
    Right redeemerRollupAScriptData -> do
      let redeemerRollupA = fromJust . V3.fromData . toPlutusData . getScriptData $ redeemerRollupAScriptData :: RollupRedeemer

      IdentityCircuitContract x ps <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "plonk-raw-contract-data.json")
      let (ledgerRules, _, _) = identityCircuitVerificationBytes x ps

      let (nextStateA, redeemerRollupB)     = nextRollup x ps redeemerRollupA

      -- let nextStateA      = toInput $ dataToBlake (rrState redeemerRollupA, rrUpdate redeemerRollupA)
      --     redeemerRollupB = nextRedeemer x ps nextStateA redeemerRollupA

      let (nextStateB, nextRedeemerRollupA) = nextRollup x ps redeemerRollupB

          -- nextStateB          = toInput $ dataToBlake (rrState redeemerRollupB, rrUpdate redeemerRollupB)
          -- nextRedeemerRollupA = nextRedeemer x ps nextStateB redeemerRollupB

      putStr $ "Verifies proof (A): " ++ (show $ rollupMini ledgerRules redeemerRollupA) ++ ".\n"
      putStr $ "Verifies proof (B): " ++ (show $ rollupMini ledgerRules redeemerRollupB)++ ".\n\n"

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
