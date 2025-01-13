module Main where

import           Cardano.Api                             hiding (Lovelace)
import           Data.Aeson                              (encode)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.String                             (IsString (fromString))
import           PlutusLedgerApi.V1.Value                (lovelaceValue)
import           PlutusLedgerApi.V3                      as V3
import           PlutusTx.Prelude                        ((<>))
import           Prelude                                 (Bool (..), Either (..), FilePath, IO, Show (..), error,
                                                          putStr, return, ($), (++))
import           System.Directory                        (createDirectoryIfMissing, getCurrentDirectory)
import           System.Environment                      (getArgs)
import           System.FilePath                         (takeFileName, (</>))
import           System.Random                           (randomRIO)
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (currencySymbolOf, dataToCBOR, dataToJSON, parseAddress,
                                                          parseTxOutRef, savePlutus)
import           ZkFold.Cardano.OnChain.BLS12_381        (F (..), toInput)
import           ZkFold.Cardano.OnChain.Utils            (dataToBlake)
import           ZkFold.Cardano.UPLC.Common              (nftPolicyCompiled, parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.Rollup              (RollupInfo (..), RollupRedeemer (..), RollupSetup (..),
                                                          rollupCompiled)
import           ZkFold.Cardano.UPLC.RollupData          (rollupDataCompiled)

rollupFee :: Lovelace
rollupFee = Lovelace 15000000

threadLovelace :: Lovelace
threadLovelace = Lovelace  3000000

saveRollupPlutus :: FilePath -> TxOutRef -> V3.Address -> IO ()
saveRollupPlutus path oref addr = do
  x  <- generate arbitrary
  ps <- generate arbitrary

  let contract = IdentityCircuitContract x ps

  BL.writeFile (path </> "test-data" </> "plonk-raw-contract-data.json") $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n"

  let (ledgerRules, iniState, _) = identityCircuitVerificationBytes x ps
      F iniState'                = iniState

      dataUpdate = [dataToBlake iniState]
      update     = [dataToBlake dataUpdate]

      protoNextState = dataToBlake (iniState, update, [] :: [V3.TxOut], lovelaceValue rollupFee)
      nextState      = toInput protoNextState

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
      rollupInfo     = RollupInfo { riDataUpdate = dataUpdate, riProtoState = protoNextState, riRedeemer = rollupRedeemer }

  let assetsPath = path </> "assets"

  savePlutus (assetsPath </> "rollup.plutus") $ rollupCompiled rollupSetup
  savePlutus (assetsPath </> "rollupData.plutus") rollupDataCompiled

  BS.writeFile (assetsPath </> "unit.cbor") $ dataToCBOR ()
  BS.writeFile (assetsPath </> "datum.cbor") $ dataToCBOR iniState'
  BS.writeFile (assetsPath </> "rollupInfo.json") $ prettyPrintJSON $ dataToJSON rollupInfo

saveParkingSpotPlutus :: FilePath -> IO ()
saveParkingSpotPlutus path = do
  randomInt <- randomRIO (1, 10000)
  savePlutus (path </> "assets" </> "parkingSpot.plutus") $ parkingSpotCompiled randomInt

saveNftPolicyPlutus :: FilePath -> TxOutRef -> IO ()
saveNftPolicyPlutus path oref = savePlutus (path </> "assets" </> "nftPolicy.plutus") $ nftPolicyCompiled oref

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
          saveRollupPlutus path nftOref addr
          saveParkingSpotPlutus path
          saveNftPolicyPlutus path nftOref

          putStr "\nDone serializing plutus scripts and initializing state.\n\n"

        Left err -> error $ "parse error: " ++ show err

    _ -> error "Error: please provide a pair of command-line string-arguments.\n"
