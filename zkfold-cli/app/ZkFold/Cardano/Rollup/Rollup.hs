module ZkFold.Cardano.Rollup.Rollup where

import           Cardano.Api                                 (getScriptData, prettyPrintJSON)
import           Cardano.Api.Shelley                         (scriptDataFromJsonDetailedSchema, toPlutusData)
import           Control.Monad                               (Monad (return), mapM, mapM_)
import           Data.Aeson                                  (decode, eitherDecode, encode)
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BL
import           Data.Maybe                                  (fromJust)
import           Data.String                                 (IsString (fromString))
import           PlutusLedgerApi.V1.Value                    (lovelaceValue)
import           PlutusLedgerApi.V3                          (Address (..), BuiltinByteString, OutputDatum (..),
                                                              TokenName (..), TxOut (..), TxOutRef, fromBuiltin,
                                                              fromData, singleton)
import           PlutusTx.Prelude                            ((<>))
import           Prelude                                     (Bool (..), Either (..), FilePath, IO, Int, Integer,
                                                              Maybe (..), Show (..), String, concatMap, error, fst,
                                                              length, map, putStr, putStrLn, read, show, unlines, zip,
                                                              ($), (++), (-), (.), (<$>))
import           System.Directory                            (createDirectoryIfMissing)
import           System.FilePath                             ((</>))
import qualified System.IO                                   as IO
import           System.Random                               (randomRIO)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (..))
import           Test.QuickCheck.Gen                         (generate)
import           Text.Printf                                 (printf)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Cardano.Examples.IdentityCircuit     (IdentityCircuitContract (..),
                                                              identityCircuitVerificationBytes,
                                                              stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils               (byteStringAsHex, credentialOf, currencySymbolOf,
                                                              dataToCBOR, dataToJSON, parseAddress, parseTxOutRef,
                                                              savePlutus)
import           ZkFold.Cardano.OnChain.BLS12_381            (F (..), bls12_381_field_prime, toInput)
import           ZkFold.Cardano.OnChain.Utils                (dataToBlake)
import qualified ZkFold.Cardano.Rollup.CardanoCli            as Cli
import           ZkFold.Cardano.Rollup.Data                  (minReq, rmax, rollupFee, threadLovelace, updateLength)
import           ZkFold.Cardano.Rollup.Example               (datumHashBSEx1, datumHashEx1, evolve)
import qualified ZkFold.Cardano.Rollup.ParseJson             as J
import           ZkFold.Cardano.UPLC.Common                  (nftPolicyCompiled, parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.Rollup                  (RollupInfo (..), RollupRedeemer (..), RollupSetup (..),
                                                              rollupCompiled)
import           ZkFold.Cardano.UPLC.RollupData              (RollupDataRedeemer (..), rollupDataCompiled)

-- | Extract keys and token names from parsed UTxOs
extractKeysAndTokens :: [J.Utxo] -> ([String], [String])
extractKeysAndTokens utxos  =
    let keys       = map J.utxoKey utxos
        tokenNames = concatMap (map fst . J.tokens . J.txOutValue . J.utxoValue) utxos
    in (keys, tokenNames)

rollupClear :: FilePath -> IO ()
rollupClear path = do
    let assets = path </> "assets"

    BS.writeFile (assets </> "dataCleanRedeemer.cbor") . dataToCBOR $ OldData

    content <- BL.readFile $ assets </> "dataTokensBurn.json"

    utxos <- case eitherDecode content of
      Left err    -> error $ "Error parsing JSON: " ++ err
      Right utxos -> return utxos

    let (keys, tokenNames) = extractKeysAndTokens utxos
        inputs = concatMap Cli.inputCodeBlock keys
        mints  = Cli.mintCodeBlock tokenNames

    putStrLn $ "\nBurning used data tokens: " ++ show tokenNames ++ "\n"

    IO.writeFile (assets </> "burnDataTokens.sh") . unlines
      $ Cli.initialCodeBlock ++ inputs ++ Cli.middleCodeBlock ++ mints ++ Cli.finalCodeBlock

saveParkingSpotPlutus :: FilePath -> Integer -> IO ()
saveParkingSpotPlutus path parkingTag = do
    let assets = path </> "assets"
    savePlutus (assets </> "parkingSpot.plutus") $ parkingSpotCompiled parkingTag
    IO.writeFile (assets </> "parkingTag.txt") $ show parkingTag

saveNftPolicyPlutus :: FilePath -> TxOutRef -> IO ()
saveNftPolicyPlutus path oref = do
    let assets = path </> "assets"
    savePlutus (assets </> "nftPolicy.plutus") $ nftPolicyCompiled oref

createRollup :: Integer -> TxOutRef -> Address -> IO (IdentityCircuitContract, RollupSetup, Integer, RollupInfo, [BuiltinByteString])
createRollup parkingTag oref addr = do
    x         <- generate arbitrary
    ps        <- generate arbitrary
    seeds     <- mapM (\_ -> randomRIO (1, rmax)) [1..updateLength]
    iniState' <- randomRIO (0, bls12_381_field_prime - 1)

    let contract = IdentityCircuitContract x ps

    let (ledgerRules, _, _) = identityCircuitVerificationBytes x ps

        dataUpdate  = map (\s -> [dataToBlake s]) seeds
        update      = dataToBlake <$> dataUpdate
        iniState    = F iniState'

    let bridgeTxOut = TxOut { txOutAddress         = Address (credentialOf $ parkingSpotCompiled parkingTag) Nothing
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

    return (contract, rollupSetup, iniState', rollupInfo, update)

saveRollupPlutus :: FilePath -> Integer -> TxOutRef -> Address -> IO ()
saveRollupPlutus path parkingTag oref addr = do
    (contract, rollupSetup, iniState', rollupInfo, update) <- createRollup parkingTag oref addr

    let testData = path </> "test-data"
        assets   = path </> "assets"

    BL.writeFile (testData </> "plonk-raw-contract-data.json") $ encode contract

    savePlutus (assets </> "rollup.plutus") $ rollupCompiled rollupSetup
    savePlutus (assets </> "rollupData.plutus") rollupDataCompiled

    BS.writeFile (assets </> "unit.cbor") $ dataToCBOR ()
    BS.writeFile (assets </> "datum.cbor") $ dataToCBOR iniState'
    BS.writeFile (assets </> "rollupInfo.json") $ prettyPrintJSON $ dataToJSON rollupInfo

    IO.writeFile (assets </> "dataTokensAmount.txt") . show . length $ update
    IO.writeFile (assets </> "bridgeDatumHash.txt") . byteStringAsHex . fromBuiltin $ datumHashBSEx1

rollupInit :: FilePath -> [String] ->  IO ()
rollupInit path args = do
    let testData = path </> "test-data"
        assets   = path </> "assets"

    createDirectoryIfMissing True testData
    createDirectoryIfMissing True assets

    let (nftOrefStr, addrStr) = case args of
          (a : b : _) -> (a, b)
          _           -> error "Error: please provide a pair of command-line arguments.\n"

    let argsE = do
          nftOref <- parseTxOutRef nftOrefStr
          addr    <- parseAddress addrStr
          return (nftOref, addr)

    let (nftOref, addr) = case argsE of
          Right a  -> a
          Left err -> error $ "parse error: " ++ show err

    parkingTag <- randomRIO (1, 10000)

    saveParkingSpotPlutus path parkingTag
    saveNftPolicyPlutus path nftOref
    saveRollupPlutus path parkingTag nftOref addr

    putStr "\nDone serializing plutus scripts and initializing state.\n\n"

-- | Compute next rollup info
nextRollup :: Fr -> Integer -> RollupInfo -> IO RollupInfo
nextRollup x parkingTag rollupInfo = do
    ps <- generate arbitrary

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

rollupUpdate :: FilePath -> IO ()
rollupUpdate path = do
    let testData = path </> "test-data"
        assets   = path </> "assets"

    rollupInfoE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assets </> "rollupInfo.json")
    parkingTag  <- read @Integer <$> IO.readFile (assets </> "parkingTag.txt")

    let rollupInfoScriptData = case rollupInfoE of
          Right a -> a
          Left _  -> error "JSON error: unreadable 'rollupInfo.json'"

    IdentityCircuitContract x _ <- fromJust . decode <$> BL.readFile (testData </> "plonk-raw-contract-data.json")

    let rollupInfo = fromJust . fromData . toPlutusData . getScriptData $ rollupInfoScriptData :: RollupInfo
    let RollupInfo dataUpdate nextState rollupRedeemer@(UpdateRollup _ update) = rollupInfo

    newRollupInfo <- nextRollup x parkingTag rollupInfo

    let F nextState' = nextState
    let dataUpdateIndexed = zip dataUpdate [1 :: Int ..]

    BS.writeFile (assets </> "datum.cbor") $ dataToCBOR nextState'
    BS.writeFile (assets </> "redeemerRollup.cbor") $ dataToCBOR rollupRedeemer
    BS.writeFile (assets </> "newRollupInfo.json") $ prettyPrintJSON $ dataToJSON newRollupInfo
    IO.writeFile (assets </> "newDataTokensAmount.txt") . show . length $ update

    mapM_ (\(dat, idx) -> BS.writeFile (assets </> printf "dataRedeemer-%02d.cbor" idx)
                          . dataToCBOR . NewData $ dat) dataUpdateIndexed
    mapM_ (\(dat, idx) -> IO.writeFile (assets </> printf "dataTokenName-%02d.txt" idx)
                          . byteStringAsHex . fromBuiltin . dataToBlake $ dat) dataUpdateIndexed

