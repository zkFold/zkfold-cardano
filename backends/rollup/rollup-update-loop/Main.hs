module Main where

import           Backend.Aux                                 (cardanoCliCode)
import           Cardano.Api                                 hiding (Lovelace, TxOut)
import           Cardano.Api.Ledger                          (toCBOR)
import           Cardano.Api.Shelley                         (fromPlutusData, scriptDataFromJsonDetailedSchema,
                                                              scriptDataToJsonDetailedSchema, toPlutusData)
import           Codec.CBOR.Write                            (toStrictByteString)
import           Data.Aeson                                  (decode)
import qualified Data.Aeson                                  as Aeson
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BL
import           Data.Maybe                                  (fromJust)
import           PlutusLedgerApi.V1.Value                    (lovelaceValue)
import           PlutusLedgerApi.V3                          as V3
import           Prelude                                     (Either (..), IO, Integer, String, concat, error, length,
                                                              read, return, show, ($), (++), (.), (<$>), (==))
import           System.Directory                            (getCurrentDirectory)
import           System.Environment                          (getArgs)
import           System.FilePath                             (takeFileName, (</>))
import qualified System.IO                                   as IO
import           Test.QuickCheck.Arbitrary                   (Arbitrary (..))
import           Test.QuickCheck.Gen                         (generate)
import           Text.Parsec                                 (many1, parse)
import           Text.Parsec.Char                            (digit)
import           Text.Parsec.String                          (Parser)
import           Text.Printf                                 (printf)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Cardano.Examples.IdentityCircuit     (stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E                 (IdentityCircuitContract (..), RollupInfo (..))
import           ZkFold.Cardano.OnChain.BLS12_381            (F (..), toInput)
import           ZkFold.Cardano.OnChain.Utils                (dataToBlake)
import           ZkFold.Cardano.UPLC.Rollup                  (RollupRedeemer (..))
import           ZkFold.Cardano.UPLC.RollupData              (RollupDataRedeemer (..))


rollupFee :: Lovelace
rollupFee = Lovelace 15000000

nextRollup :: Fr -> RollupInfo -> IO RollupInfo
nextRollup x rollupInfo = do
  ps <- generate arbitrary

  -- putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n\n"

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

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let currentDirName = takeFileName currentDir
      path           = if currentDirName == "rollup" then (".." </> "..")
                          else if currentDirName == "e2e-test" then ".." else "."
      assetsPath     = path </> "assets"

  args        <- getArgs
  rollupInfoE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assetsPath </> "rollupInfo.json")

  case args of
    [nStr] -> do
      let nE = parse integerParser "" nStr
      case nE of
        Right n -> do
          case rollupInfoE of
            Right rollupInfoScriptData -> do
              IdentityCircuitContract x _ <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "plonk-raw-contract-data.json")

              let rollupInfo = fromJust . fromData . toPlutusData . getScriptData $ rollupInfoScriptData :: RollupInfo

              let RollupInfo dataUpdate protoNextState rollupRedeemer@(UpdateRollup _ update) = rollupInfo

              newRollupInfo <- nextRollup x rollupInfo

              let nextState    = toInput protoNextState
                  F nextState' = nextState

              let rollupDataRedeemer = NewData dataUpdate

              BS.writeFile (assetsPath </> "dataRedeemer.cbor") $ dataToCBOR rollupDataRedeemer
              BS.writeFile (assetsPath </> "datum.cbor") $ dataToCBOR nextState'
              BS.writeFile (assetsPath </> "redeemerRollup.cbor") $ dataToCBOR rollupRedeemer

              BS.writeFile (assetsPath </> "newRollupInfo.json") $ prettyPrintJSON $ dataToJSON newRollupInfo

              IO.writeFile (assetsPath </> "dataTokenName.txt") . byteStringAsHex . fromBuiltin . dataToBlake $ dataUpdate
              IO.writeFile (assetsPath </> "dataUpdateLength.txt") . show . length $ update
              IO.writeFile (assetsPath </> "rollupCLICode.sh") $ cardanoCliCode n

            Left _                     -> error "JSON error: unreadable 'rollupInfo.json'"

        Left err -> error $ "parse error: " ++ show err

    _      -> error "Usage: cabal run rollup-update-loop -- <N>"

----- HELPER FUNCTIONS -----

-- | Serialise data to CBOR and then wrap it in a JSON object.
dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . toData

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . toData

-- | Get hex representation of bytestring
byteStringAsHex :: BS.ByteString -> String
byteStringAsHex bs = concat $ BS.foldr' (\w s -> (printf "%02x" w):s) [] bs

-- | Parser for a positive integer
integerParser :: Parser Integer
integerParser = do
  digits <- many1 digit
  return $ read digits
