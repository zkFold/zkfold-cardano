module Main where

import           Cardano.Api                    hiding (Key, Lovelace, TxOut)
import           Cardano.Api.Ledger             (toCBOR)
import           Cardano.Api.Shelley            (PlutusScript (..), fromPlutusData)
import           Codec.CBOR.Write               (toStrictByteString)
import           Data.Aeson                     (eitherDecode)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import qualified PlutusLedgerApi.V3             as V3
import           PlutusTx                       (CompiledCode, ToData, toData)
import           Prelude                        (Either (..), IO, String, concatMap, fst, map, putStrLn, show, unlines,
                                                 ($), (++), (.))
import qualified Rollup.CardanoCli              as Cli
import qualified Rollup.ParseJson               as J
import           System.Directory               (getCurrentDirectory)
import           System.FilePath                (takeFileName, (</>))
import qualified System.IO                      as IO

import           ZkFold.Cardano.UPLC.RollupData (RollupDataRedeemer (..))


-- | Extract keys and token names from parsed UTxOs
extractKeysAndTokens :: [J.Utxo] -> ([String], [String])
extractKeysAndTokens utxos  =
  let keys       = map J.utxoKey utxos
      tokenNames = concatMap (map fst . J.tokens . J.txOutValue . J.utxoValue) utxos
  in (keys, tokenNames)

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path       = case takeFileName currentDir of
                     "rollup"   -> ".." </> ".."
                     "e2e-test" -> ".."
                     _          -> "."
      assetsPath = path </> "assets"

  BS.writeFile (assetsPath </> "dataCleanRedeemer.cbor") . dataToCBOR $ OldData

  content <- BL.readFile $ assetsPath </> "dataTokensBurn.json"

  case eitherDecode content of
    Left err    -> putStrLn $ "Error parsing JSON: " ++ err
    Right utxos -> do
      let (keys, tokenNames) = extractKeysAndTokens utxos
          inputs = concatMap Cli.inputCodeBlock keys
          mints  = Cli.mintCodeBlock tokenNames

      putStrLn $ "\nBurning used data tokens: " ++ show tokenNames ++ "\n"

      IO.writeFile (assetsPath </> "burnDataTokens.sh") . unlines
        $ Cli.initialCodeBlock ++ inputs ++ Cli.middleCodeBlock ++ mints ++ Cli.finalCodeBlock


----- HELPER FUNCTIONS -----

-- | Script hash of compiled validator
scriptHashOf :: CompiledCode a -> V3.ScriptHash
scriptHashOf = V3.ScriptHash . V3.toBuiltin . serialiseToRawBytes . hashScript . PlutusScript plutusScriptVersion
               . PlutusScriptSerialised @PlutusScriptV3 . V3.serialiseCompiledCode

-- | Serialise data to CBOR.
dataToCBOR :: ToData a => a -> BS.ByteString
dataToCBOR = toStrictByteString . toCBOR . fromPlutusData . toData
