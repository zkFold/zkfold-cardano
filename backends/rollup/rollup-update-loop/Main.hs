module Main where

import           Cardano.Api                                 hiding (Lovelace, TxOut)
import           Cardano.Api.Ledger                          (toCBOR)
import           Cardano.Api.Shelley                         (PlutusScript (..), fromPlutusData,
                                                              scriptDataFromJsonDetailedSchema,
                                                              scriptDataToJsonDetailedSchema, toPlutusData)
import           Codec.CBOR.Write                            (toStrictByteString)
import           Data.Aeson                                  (decode)
import qualified Data.Aeson                                  as Aeson
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BL
import           Data.Maybe                                  (fromJust)
import           PlutusLedgerApi.V1.Value                    (lovelaceValue)
import           PlutusLedgerApi.V3                          as V3
import           PlutusTx                                    (CompiledCode)
import           Prelude                                     (Either (..), FilePath, IO, String, concat, drop, error, length, replicate,
                                                              return, show, take, zipWith, ($), (++), (-), (.), (<$>), (==))
import           System.Directory                            (getCurrentDirectory)
import           System.FilePath                             (takeFileName, (</>))
import qualified System.IO                                   as IO
import           Test.QuickCheck.Arbitrary                   (Arbitrary (..))
import           Test.QuickCheck.Gen                         (generate)
import           Text.Printf                                 (printf)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Cardano.Examples.IdentityCircuit     (stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E                 (IdentityCircuitContract (..), RollupInfo (..))
import           ZkFold.Cardano.OnChain.BLS12_381            (F (..), toInput)
import           ZkFold.Cardano.OnChain.Utils                (dataToBlake)
import           ZkFold.Cardano.UPLC                         (rollupDataCompiled)
import           ZkFold.Cardano.UPLC.Rollup                  (RollupRedeemer (..))
import           ZkFold.Cardano.UPLC.RollupData              (RollupDataRedeemer (..))

rollupFee :: Lovelace
rollupFee = Lovelace 15000000

-- | Compute next rollup info
nextRollup :: FilePath -> Fr -> RollupInfo -> IO RollupInfo
nextRollup path x rollupInfo = do
  ps <- generate arbitrary

  -- putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n\n"

  let updateLength = 1

  let dataUpdate1            = riDataUpdate rollupInfo
      protoState1            = riProtoState rollupInfo
      UpdateRollup _ update1 = riRedeemer   rollupInfo

      state1      = toInput protoState1

      dataUpdate2 = protoState1 : dataUpdate1
      preUpdate2  = dataToBlake dataUpdate2 : update1
      update2     = take updateLength preUpdate2
      protoState2 = dataToBlake (state1, update2, [] :: [TxOut], lovelaceValue rollupFee)

      state2      = toInput protoState2

      (_, _, proof2)  = stateCheckVerificationBytes x ps state2
      rollupRedeemer2 = UpdateRollup proof2 update2

  IO.writeFile (path </> "newDataTokens.txt") $ toDataTokens update2
  IO.writeFile (path </> "newDataTokensDiscarded.txt") $ toDataTokens $ drop updateLength preUpdate2
  IO.writeFile (path </> "newDataTokensAmount.txt") . show . length $ update2

  return $ RollupInfo dataUpdate2 protoState2 rollupRedeemer2

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let currentDirName = takeFileName currentDir
      path           = if currentDirName == "rollup" then (".." </> "..")
                          else if currentDirName == "e2e-test" then ".." else "."
      assetsPath     = path </> "assets"

  rollupInfoE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assetsPath </> "rollupInfo.json")

  case rollupInfoE of
    Right rollupInfoScriptData -> do
      IdentityCircuitContract x _ <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "plonk-raw-contract-data.json")

      let rollupInfo = fromJust . fromData . toPlutusData . getScriptData $ rollupInfoScriptData :: RollupInfo

      let RollupInfo dataUpdate protoNextState rollupRedeemer = rollupInfo

      newRollupInfo <- nextRollup assetsPath x rollupInfo

      let nextState    = toInput protoNextState
          F nextState' = nextState

      let rollupDataRedeemer = NewData dataUpdate

      BS.writeFile (assetsPath </> "datum.cbor") $ dataToCBOR nextState'
      BS.writeFile (assetsPath </> "dataRedeemer.cbor") $ dataToCBOR rollupDataRedeemer
      BS.writeFile (assetsPath </> "redeemerRollup.cbor") $ dataToCBOR rollupRedeemer

      BS.writeFile (assetsPath </> "newRollupInfo.json") $ prettyPrintJSON $ dataToJSON newRollupInfo

      IO.writeFile (assetsPath </> "dataNewTokenName.txt") . byteStringAsHex . fromBuiltin . dataToBlake $ dataUpdate

    Left _                     -> error "JSON error: unreadable 'rollupInfo.json'"


----- HELPER FUNCTIONS -----

-- | Get hex representation of bytestring
byteStringAsHex :: BS.ByteString -> String
byteStringAsHex bs = concat $ BS.foldr' (\w s -> (printf "%02x" w):s) [] bs

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . toData

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . toData

-- | Script hash of compiled validator
scriptHashOf :: CompiledCode a -> V3.ScriptHash
scriptHashOf = V3.ScriptHash . toBuiltin . serialiseToRawBytes . hashScript . PlutusScript plutusScriptVersion
               . PlutusScriptSerialised @PlutusScriptV3 . serialiseCompiledCode

-- | String of data tokens to be used by cardano-cli
toDataTokens :: [BuiltinByteString] -> String
toDataTokens update = concat $ zipWith zipper update wrapups
    where
      zipper bbs s = "1 " ++ (show $ scriptHashOf rollupDataCompiled) ++ "." ++ (byteStringAsHex $ fromBuiltin bbs) ++ s
      wrapups      = replicate (length update - 1) " + " ++ [""]

