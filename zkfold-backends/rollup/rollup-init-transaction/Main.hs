module Main where

import           Cardano.Api                             hiding (Lovelace)
import           Control.Monad                           (mapM)
import           Data.Aeson                              (encode)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.String                             (IsString (fromString))
import           PlutusLedgerApi.V1.Value                (lovelaceValue)
import           PlutusLedgerApi.V3                      as V3
import           PlutusTx.Prelude                        ((<>))
import           Prelude                                 (Bool (..), Either (..), FilePath, IO, Int, Integer,
                                                          Maybe (..), Show (..), error,
                                                          length, map, putStr, return, ($), (++), (-), (.),
                                                          (<$>))
import           Rollup.Example                          (datumHashBSEx1, datumHashEx1)
import           System.Directory                        (createDirectoryIfMissing, getCurrentDirectory)
import           System.Environment                      (getArgs)
import           System.FilePath                         (takeFileName, (</>))
import qualified System.IO                               as IO
import           System.Random                           (randomRIO)
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (currencySymbolOf, dataToCBOR, dataToJSON, parseAddress,
                                                          parseTxOutRef, savePlutus, credentialOf, byteStringAsHex)
import           ZkFold.Cardano.OnChain.BLS12_381        (F (..), bls12_381_field_prime, toInput)
import           ZkFold.Cardano.OnChain.Utils            (dataToBlake)
import           ZkFold.Cardano.UPLC.Common              (nftPolicyCompiled, parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.Rollup              (RollupInfo (..), RollupRedeemer (..), RollupSetup (..),
                                                          rollupCompiled)
import           ZkFold.Cardano.UPLC.RollupData          (rollupDataCompiled)

rollupFee :: Lovelace
rollupFee = Lovelace 15000000

threadLovelace :: Lovelace
threadLovelace = Lovelace 3000000

updateLength :: Int
updateLength = 3

rmax :: Integer
rmax = 1000

minReq :: Lovelace
minReq = Lovelace 995610

saveParkingSpotPlutus :: FilePath -> Integer -> IO ()
saveParkingSpotPlutus path parkingTag = do
  savePlutus (path </> "assets" </> "parkingSpot.plutus") $ parkingSpotCompiled parkingTag
  IO.writeFile (path </> "assets" </> "parkingTag.txt") $ show parkingTag

saveNftPolicyPlutus :: FilePath -> TxOutRef -> IO ()
saveNftPolicyPlutus path oref = savePlutus (path </> "assets" </> "nftPolicy.plutus") $ nftPolicyCompiled oref

saveRollupPlutus :: FilePath -> Integer -> TxOutRef -> V3.Address -> IO ()
saveRollupPlutus path parkingTag oref addr = do
  x         <- generate arbitrary
  ps        <- generate arbitrary
  seeds     <- mapM (\_ -> randomRIO (1, rmax)) [1..updateLength]
  iniState' <- randomRIO (0, bls12_381_field_prime - 1)

  let contract = IdentityCircuitContract x ps

  BL.writeFile (path </> "test-data" </> "plonk-raw-contract-data.json") $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n"

  let (ledgerRules, _, _) = identityCircuitVerificationBytes x ps

      dataUpdate  = map (\s -> [dataToBlake s]) seeds
      update      = dataToBlake <$> dataUpdate
      iniState    = F iniState'

  let bridgeTxOut = V3.TxOut { txOutAddress         = Address (credentialOf $ parkingSpotCompiled parkingTag) Nothing
                             , txOutValue           = lovelaceValue minReq
                             , txOutDatum           = OutputDatumHash datumHashEx1
                             , txOutReferenceScript = Nothing
                             }

      nextState = toInput $ dataToBlake (iniState, update, [bridgeTxOut], lovelaceValue rollupFee)

      (_, _, proof) = stateCheckVerificationBytes x ps nextState

  let threadCS    = currencySymbolOf $ nftPolicyCompiled oref
      threadName  = TokenName (fromString "zkFold" :: BuiltinByteString)
      rollupSetup = RollupSetup
                    { rsLedgerRules  = ledgerRules
                    , rsDataCurrency = currencySymbolOf rollupDataCompiled
                    , rsThreadValue  = lovelaceValue threadLovelace <> singleton threadCS threadName 1
                    , rsFeeAddress   = addr
                    }

  let rollupRedeemer = UpdateRollup proof update
      rollupInfo     = RollupInfo { riDataUpdate = dataUpdate, riState = nextState, riRedeemer = rollupRedeemer }

  let assetsPath = path </> "assets"

  savePlutus (assetsPath </> "rollup.plutus") $ rollupCompiled rollupSetup
  savePlutus (assetsPath </> "rollupData.plutus") rollupDataCompiled

  BS.writeFile (assetsPath </> "unit.cbor") $ dataToCBOR ()
  BS.writeFile (assetsPath </> "datum.cbor") $ dataToCBOR iniState'
  BS.writeFile (assetsPath </> "rollupInfo.json") $ prettyPrintJSON $ dataToJSON rollupInfo

  IO.writeFile (assetsPath </> "dataTokensAmount.txt") . show . length $ update
  IO.writeFile (assetsPath </> "bridgeDatumHash.txt") . byteStringAsHex . fromBuiltin $ datumHashBSEx1

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "rollup"  -> ".." </> ".."
        "scripts" -> ".."
        _         -> "."

  createDirectoryIfMissing True $ path </> "test-data"
  createDirectoryIfMissing True $ path </> "assets"

  argsRaw <- getArgs

  case argsRaw of
    (nftOrefStr : addrStr : _) -> do
      let argsE = do
            nftOref <- parseTxOutRef nftOrefStr
            addr    <- parseAddress addrStr
            return (nftOref, addr)
      case argsE of
        Right (nftOref, addr) -> do
          parkingTag <- randomRIO (1, 10000)

          saveParkingSpotPlutus path parkingTag
          saveNftPolicyPlutus path nftOref
          saveRollupPlutus path parkingTag nftOref addr

          putStr "\nDone serializing plutus scripts and initializing state.\n\n"

        Left err -> error $ "parse error: " ++ show err

    _ -> error "Error: please provide a pair of command-line arguments.\n"
