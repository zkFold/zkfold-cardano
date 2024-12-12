module Main where

import           Data.String                             (IsString (fromString))
-- import           PlutusTx.Builtins                       (byteStringToInteger)
import           PlutusLedgerApi.V1.Value                (lovelaceValue)
-- import           GHC.ByteOrder                           (ByteOrder(..))

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
import           Prelude                                     (Either (..), IO, error, return,
                                                              ($), (.), (<$>), (>>), (==))
import           System.Directory                            (getCurrentDirectory)
import           System.Exit                                 (exitFailure)
import           System.FilePath                             (takeFileName, (</>))
-- import qualified System.IO                                   as IO
import           Test.QuickCheck.Arbitrary                   (Arbitrary (..))
import           Test.QuickCheck.Gen                         (generate)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Cardano.Examples.IdentityCircuit     (stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E                 (IdentityCircuitContract (..), RollupInfo (..))
import           ZkFold.Cardano.OnChain.BLS12_381            (F (..), toInput)
import           ZkFold.Cardano.OnChain.Utils                (dataToBlake)
import           ZkFold.Cardano.UPLC.Rollup                  (RollupRedeemer (..))

nextRollup :: Fr -> RollupInfo -> IO RollupInfo
nextRollup x rollupInfo = do
  ps <- generate arbitrary

  -- putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n\n"

  let nextState1 = riNextState rollupInfo
      dataUpdate = dataToBlake [fromString "deadbeef" :: V3.BuiltinByteString]
      nextUpdate = [dataUpdate]
      nextState2 = toInput $ dataToBlake ( nextState1
                                         , nextUpdate
                                         , [] :: [V3.TxOut]
                                         , lovelaceValue $ V3.Lovelace 15000000
                                         )

  let (_, _, proof2)  = stateCheckVerificationBytes x ps nextState2
      rollupRedeemer2 = UpdateRollup proof2 nextUpdate

  return $ RollupInfo nextState2 rollupRedeemer2


-- | Will process two simultaneous transactions 'A' & 'B', uploading states 'nextStateA', 'nextStateB'
-- with redeemers 'redeemerRollupA', 'redeemerRollupB', respectively.

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let currentDirName = takeFileName currentDir
      path           = if currentDirName == "rollup" then (".." </> "..")
                          else if currentDirName == "e2e-test" then ".." else "."
      assetsPath     = path </> "assets"

  rollupInfoAE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assetsPath </> "rollupInfoA.json")

  case rollupInfoAE of
    Right rollupInfoAScriptData -> do
      IdentityCircuitContract x _ <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "plonk-raw-contract-data.json")

      let rollupInfoA = fromJust . V3.fromData . toPlutusData . getScriptData $ rollupInfoAScriptData :: RollupInfo

      let RollupInfo nextStateA redeemerRollupA = rollupInfoA

      rollupInfoB@(RollupInfo nextStateB redeemerRollupB) <- nextRollup x rollupInfoA
      newRollupInfoA                                      <- nextRollup x rollupInfoB

      let F nextStateA' = nextStateA
          F nextStateB' = nextStateB

      BS.writeFile (assetsPath </> "datumA.cbor") $ dataToCBOR nextStateA'
      BS.writeFile (assetsPath </> "redeemerRollupA.cbor") $ dataToCBOR redeemerRollupA

      BS.writeFile (assetsPath </> "datumB.cbor") $ dataToCBOR nextStateB'
      BS.writeFile (assetsPath </> "redeemerRollupB.cbor") $ dataToCBOR redeemerRollupB
      -- IO.writeFile (assetsPath </> "last-update-length.log") . show . length . rrUpdate $ redeemerRollupB

      BS.writeFile (assetsPath </> "newRollupInfoA.json") $ prettyPrintJSON $ dataToJSON newRollupInfoA

    Left _                      -> error "JSON error: unreadable 'rollupInfoA.json'" >> exitFailure


----- HELPER FUNCTIONS -----

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . V3.toData
