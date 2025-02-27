module ZkFold.Cardano.Rollup.Transaction.Update where

import           Cardano.Api                      (getScriptData)
import           Cardano.Api.Shelley              (scriptDataFromJsonDetailedSchema, toPlutusData)
import           Control.Monad                    (mapM_)
import           Data.Aeson                       (decode)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL
import           Data.Maybe                       (fromJust)
import           PlutusLedgerApi.V3               (fromBuiltin, fromData)
import           Prelude                          (Either (..), FilePath, IO, Int, Show (..), error, length, show, zip,
                                                   ($), (.), (<$>))
import           System.FilePath                  ((</>))
import qualified System.IO                        as IO
import           Text.Printf                      (printf)

import           ZkFold.Cardano.OffChain.Utils    (byteStringAsHex, dataToCBOR)
import           ZkFold.Cardano.OnChain.BLS12_381 (F (..))
import           ZkFold.Cardano.OnChain.Utils     (dataToBlake)
import           ZkFold.Cardano.UPLC.Rollup       (RollupInfo (..), RollupRedeemer (..))
import           ZkFold.Cardano.UPLC.RollupData   (RollupDataRedeemer (..))

rollupUpdate :: FilePath -> IO ()
rollupUpdate path = do
    let assets   = path </> "assets"

    rollupInfoE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assets </> "rollupInfo.json")

    let rollupInfoScriptData = case rollupInfoE of
          Right a -> a
          Left _  -> error "JSON error: unreadable 'rollupInfo.json'"

    let rollupInfo = fromJust . fromData . toPlutusData . getScriptData $ rollupInfoScriptData :: RollupInfo
    let RollupInfo _ dataUpdate nextState rollupRedeemer@(UpdateRollup _ update) = rollupInfo

    let F nextState' = nextState
    let dataUpdateIndexed = zip dataUpdate [1 :: Int ..]

    BS.writeFile (assets </> "datum.cbor") $ dataToCBOR nextState'
    BS.writeFile (assets </> "redeemerRollup.cbor") $ dataToCBOR rollupRedeemer

    IO.writeFile (assets </> "newDataTokensAmount.txt") . show . length $ update

    mapM_ (\(dat, idx) -> BS.writeFile (assets </> printf "dataRedeemer-%02d.cbor" idx)
                          . dataToCBOR . NewData $ dat) dataUpdateIndexed
    mapM_ (\(dat, idx) -> IO.writeFile (assets </> printf "dataTokenName-%02d.txt" idx)
                          . byteStringAsHex . fromBuiltin . dataToBlake $ dat) dataUpdateIndexed
