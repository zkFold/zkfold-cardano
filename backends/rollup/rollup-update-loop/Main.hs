module Main where

import           Cardano.Api                                 hiding (Lovelace, TxOut)
import           Cardano.Api.Ledger                          (toCBOR)
import           Cardano.Api.Shelley                         (PlutusScript (..), fromPlutusData,
                                                              scriptDataFromJsonDetailedSchema,
                                                              scriptDataToJsonDetailedSchema, toPlutusData)
import           Codec.CBOR.Write                            (toStrictByteString)
import           Control.Monad
import           Data.Aeson                                  (decode)
import qualified Data.Aeson                                  as Aeson
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BL
import           Data.Maybe                                  (fromJust)
import           PlutusLedgerApi.V1.Value                    (lovelaceValue)
import           PlutusLedgerApi.V3                          as V3
import           PlutusTx                                    (CompiledCode)
import           Prelude                                     (Either (..), IO, Int, Integer, Maybe (..), String, concat, error,
                                                              length, read, replicate, show, zip, zipWith, ($), (++), (-),
                                                              (.), (<$>), (==))
import           Rollup.Example                              (datumHashEx1, evolve)
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
import           ZkFold.Cardano.UPLC                         (parkingSpotCompiled, rollupDataCompiled)
import           ZkFold.Cardano.UPLC.Rollup                  (RollupRedeemer (..))
import           ZkFold.Cardano.UPLC.RollupData              (RollupDataRedeemer (..))

rollupFee, minReq :: Lovelace
rollupFee = Lovelace 15000000
minReq    = Lovelace   995610    

rmax :: Integer
rmax = 1000

-- | Compute next rollup info
nextRollup :: Fr -> Integer -> RollupInfo -> IO RollupInfo
nextRollup x parkingTag rollupInfo = do
  ps <- generate arbitrary

  -- putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n\n"

  let dataUpdate1 = riDataUpdate rollupInfo
      state1      = riState      rollupInfo
      -- protoState1 = riProtoState rollupInfo

      -- state1 = toInput protoState1

  dataUpdate2 <- mapM evolve dataUpdate1

  let bridgeTxOut = TxOut { txOutAddress         = Address (credentialOf $ parkingSpotCompiled parkingTag) Nothing
                          , txOutValue           = lovelaceValue minReq
                          , txOutDatum           = OutputDatumHash datumHashEx1
                          , txOutReferenceScript = Nothing
                          }

  let update2 = dataToBlake <$> dataUpdate2
      state2  = toInput $ dataToBlake (state1, update2, [bridgeTxOut], lovelaceValue rollupFee)

      -- state2 = toInput protoState2

      (_, _, proof2)  = stateCheckVerificationBytes x ps state2
      rollupRedeemer2 = UpdateRollup proof2 update2

  return $ RollupInfo dataUpdate2 state2 rollupRedeemer2

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let currentDirName = takeFileName currentDir
      path           = if currentDirName == "rollup" then (".." </> "..")
                          else if currentDirName == "e2e-test" then ".." else "."
      assetsPath     = path </> "assets"

  rollupInfoE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assetsPath </> "rollupInfo.json")
  parkingTag  <- read @Integer <$> IO.readFile (assetsPath </> "parkingTag.txt")

  case rollupInfoE of
    Right rollupInfoScriptData -> do
      IdentityCircuitContract x _ <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "plonk-raw-contract-data.json")

      let rollupInfo = fromJust . fromData . toPlutusData . getScriptData $ rollupInfoScriptData :: RollupInfo

      let RollupInfo dataUpdate nextState rollupRedeemer@(UpdateRollup _ update) = rollupInfo

      newRollupInfo <- nextRollup x parkingTag rollupInfo

      -- let nextState    = toInput protoNextState
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

-- | Credential of compiled validator script
credentialOf :: CompiledCode a -> Credential
credentialOf = ScriptCredential . V3.ScriptHash . toBuiltin . serialiseToRawBytes . hashScript
               . PlutusScript plutusScriptVersion . PlutusScriptSerialised @PlutusScriptV3 . serialiseCompiledCode

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
