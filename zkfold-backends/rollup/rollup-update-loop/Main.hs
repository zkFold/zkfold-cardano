module Main where

import           Cardano.Api                                 hiding (Lovelace, TxOut)
import           Cardano.Api.Shelley                         (scriptDataFromJsonDetailedSchema, toPlutusData)
import           Control.Monad
import           Data.Aeson                                  (decode)
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BL
import           Data.Maybe                                  (fromJust)
import           PlutusLedgerApi.V1.Value                    (lovelaceValue)
import           PlutusLedgerApi.V3                          as V3
import           Prelude                                     (Either (..), IO, Int, Integer, Maybe (..),
                                                              error, length, read, show, zip, ($), (.), (<$>), (==))
import           Rollup.Example                              (datumHashEx1, evolve)
import           System.Directory                            (getCurrentDirectory)
import           System.FilePath                             (takeFileName, (</>))
import qualified System.IO                                   as IO
import           Test.QuickCheck.Arbitrary                   (Arbitrary (..))
import           Test.QuickCheck.Gen                         (generate)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           Text.Printf          (printf)
import           ZkFold.Cardano.Examples.IdentityCircuit     (IdentityCircuitContract (..), stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils               (byteStringAsHex, dataToCBOR, dataToJSON, credentialOf)
import           ZkFold.Cardano.OnChain.BLS12_381            (F (..), toInput)
import           ZkFold.Cardano.OnChain.Utils                (dataToBlake)
import           ZkFold.Cardano.UPLC.Common                  (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.Rollup                  (RollupRedeemer (..), RollupInfo(..))
import           ZkFold.Cardano.UPLC.RollupData              (RollupDataRedeemer (..))

rollupFee, minReq :: Lovelace
rollupFee = Lovelace 15000000
minReq    = Lovelace   995610

-- | Compute next rollup info
nextRollup :: Fr -> Integer -> RollupInfo -> IO RollupInfo
nextRollup x parkingTag rollupInfo = do
  ps <- generate arbitrary

  -- putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n\n"

  let dataUpdate1 = riDataUpdate rollupInfo
      state1      = riState      rollupInfo

  dataUpdate2 <- mapM evolve dataUpdate1

  let bridgeTxOut = TxOut { txOutAddress         = Address (credentialOf $ parkingSpotCompiled parkingTag) Nothing
                          , txOutValue           = lovelaceValue minReq
                          , txOutDatum           = OutputDatumHash datumHashEx1
                          , txOutReferenceScript = Nothing
                          }

  let update2 = dataToBlake <$> dataUpdate2
      state2  = toInput $ dataToBlake (state1, update2, [bridgeTxOut], lovelaceValue rollupFee)

      (_, _, proof2)  = stateCheckVerificationBytes x ps state2
      rollupRedeemer2 = UpdateRollup proof2 update2

  return $ RollupInfo dataUpdate2 state2 rollupRedeemer2

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let currentDirName = takeFileName currentDir
      path           = if currentDirName == "rollup" then (".." </> "..")
                          else if currentDirName == "scripts" then ".." else "."
      assetsPath     = path </> "assets"

  rollupInfoE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assetsPath </> "rollupInfo.json")
  parkingTag  <- read @Integer <$> IO.readFile (assetsPath </> "parkingTag.txt")

  case rollupInfoE of
    Right rollupInfoScriptData -> do
      IdentityCircuitContract x _ <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "plonk-raw-contract-data.json")

      let rollupInfo = fromJust . fromData . toPlutusData . getScriptData $ rollupInfoScriptData :: RollupInfo

      let RollupInfo dataUpdate nextState rollupRedeemer@(UpdateRollup _ update) = rollupInfo

      newRollupInfo <- nextRollup x parkingTag rollupInfo

      let F nextState' = nextState

      let dataUpdateIndexed = zip dataUpdate [1 :: Int ..]

      BS.writeFile (assetsPath </> "datum.cbor") $ dataToCBOR nextState'

      BS.writeFile (assetsPath </> "redeemerRollup.cbor") $ dataToCBOR rollupRedeemer

      BS.writeFile (assetsPath </> "newRollupInfo.json") $ prettyPrintJSON $ dataToJSON newRollupInfo

      IO.writeFile (assetsPath </> "newDataTokensAmount.txt") . show . length $ update

      mapM_ (\(dat, idx) -> BS.writeFile (assetsPath </> (printf "dataRedeemer-%02d.cbor" idx))
                            . dataToCBOR . NewData $ dat) dataUpdateIndexed

      mapM_ (\(dat, idx) -> IO.writeFile (assetsPath </> (printf "dataTokenName-%02d.txt" idx))
                            . byteStringAsHex . fromBuiltin . dataToBlake $ dat) dataUpdateIndexed

    Left _                     -> error "JSON error: unreadable 'rollupInfo.json'"
