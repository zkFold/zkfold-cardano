module Main where

import           Cardano.Api                                 hiding (Lovelace, TxOut)
import           Cardano.Api.Shelley                         (scriptDataFromJsonDetailedSchema, toPlutusData)
import           Data.Aeson                                  (decode)
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BL
import           Data.Maybe                                  (fromJust)
import           PlutusLedgerApi.V1.Value                    (lovelaceValue)
import           PlutusLedgerApi.V3                          as V3
import           Prelude                                     (Either (..), IO, String, concat, error, length, replicate,
                                                              return, show, zipWith, ($), (++), (-), (.), (<$>), (==))
import           System.Directory                            (getCurrentDirectory)
import           System.FilePath                             (takeFileName, (</>))
import qualified System.IO                                   as IO
import           Test.QuickCheck.Arbitrary                   (Arbitrary (..))
import           Test.QuickCheck.Gen                         (generate)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Cardano.Examples.IdentityCircuit     (IdentityCircuitContract (..), stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils               (byteStringAsHex, dataToCBOR, dataToJSON, scriptHashOf)
import           ZkFold.Cardano.OnChain.BLS12_381            (F (..), toInput)
import           ZkFold.Cardano.OnChain.Utils                (dataToBlake)
import           ZkFold.Cardano.UPLC.Rollup                  (RollupInfo (..), RollupRedeemer (..))
import           ZkFold.Cardano.UPLC.RollupData              (RollupDataRedeemer (..), rollupDataCompiled)

rollupFee :: Lovelace
rollupFee = Lovelace 15000000

-- | Compute next rollup info
nextRollup :: Fr -> RollupInfo -> IO RollupInfo
nextRollup x rollupInfo = do
  ps <- generate arbitrary

  let dataUpdate1            = riDataUpdate rollupInfo
      protoState1            = riProtoState rollupInfo
      UpdateRollup _ update1 = riRedeemer   rollupInfo

      state1      = toInput protoState1

      dataUpdate2 = protoState1 : dataUpdate1
      update2     = dataToBlake dataUpdate2 : update1
      protoState2 = dataToBlake (state1, update2, [] :: [TxOut], lovelaceValue rollupFee)

      state2      = toInput protoState2

      (_, _, proof2)  = stateCheckVerificationBytes x ps state2
      rollupRedeemer2 = UpdateRollup proof2 update2

  return $ RollupInfo dataUpdate2 protoState2 rollupRedeemer2

-- | String of data tokens to be used by cardano-cli
toDataTokens :: [BuiltinByteString] -> String
toDataTokens update = concat $ zipWith zipper update wrapups
    where
      zipper bbs s = "1 " ++ (show $ scriptHashOf rollupDataCompiled) ++ "." ++ (byteStringAsHex $ fromBuiltin bbs) ++ s
      wrapups      = replicate (length update - 1) " + " ++ [""]

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let currentDirName = takeFileName currentDir
      path           = if currentDirName == "rollup" then (".." </> "..")
                          else if currentDirName == "scripts" then ".." else "."
      assetsPath     = path </> "assets"

  rollupInfoE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assetsPath </> "rollupInfo.json")

  case rollupInfoE of
    Right rollupInfoScriptData -> do
      IdentityCircuitContract x _ <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "plonk-raw-contract-data.json")

      let rollupInfo = fromJust . fromData . toPlutusData . getScriptData $ rollupInfoScriptData :: RollupInfo

      let RollupInfo dataUpdate protoNextState rollupRedeemer@(UpdateRollup _ update) = rollupInfo

      newRollupInfo <- nextRollup x rollupInfo

      let nextState    = toInput protoNextState
          F nextState' = nextState

      let rollupDataRedeemer = NewData dataUpdate

      BS.writeFile (assetsPath </> "datum.cbor") $ dataToCBOR nextState'
      BS.writeFile (assetsPath </> "dataRedeemer.cbor") $ dataToCBOR rollupDataRedeemer
      BS.writeFile (assetsPath </> "redeemerRollup.cbor") $ dataToCBOR rollupRedeemer

      BS.writeFile (assetsPath </> "newRollupInfo.json") $ prettyPrintJSON $ dataToJSON newRollupInfo

      IO.writeFile (assetsPath </> "dataNewTokenName.txt") . byteStringAsHex . fromBuiltin . dataToBlake $ dataUpdate
      IO.writeFile (assetsPath </> "dataTokens.txt") $ toDataTokens update
      IO.writeFile (assetsPath </> "dataUpdateLength.txt") . show . length $ update

    Left _                     -> error "JSON error: unreadable 'rollupInfo.json'"
