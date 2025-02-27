module ZkFold.Cardano.Rollup.Transaction.Next where

import           Cardano.Api                                 (getScriptData, prettyPrintJSON)
import           Cardano.Api.Shelley                         (scriptDataFromJsonDetailedSchema, toPlutusData)
import           Control.Monad                               (Monad (..), mapM)
import           Data.Aeson                                  (decode)
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BL
import           Data.Maybe                                  (fromJust)
import           PlutusLedgerApi.V1.Value                    (lovelaceValue)
import           PlutusLedgerApi.V3                          (Address (..), OutputDatum (..), TxOut (..),
                                                              fromData)
import           Prelude                                     (Either (..), FilePath, IO, Integer,
                                                              Maybe (..), error, read,
                                                              ($), (.), (<$>))
import           System.FilePath                             ((</>))
import qualified System.IO                                   as IO
import           Test.QuickCheck.Arbitrary                   (Arbitrary (..))
import           Test.QuickCheck.Gen                         (generate)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Cardano.Examples.IdentityCircuit     (IdentityCircuitContract (..),
                                                              stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils               (credentialOf, dataToJSON)
import           ZkFold.Cardano.OnChain.BLS12_381            (toInput)
import           ZkFold.Cardano.OnChain.Utils                (dataToBlake)
import           ZkFold.Cardano.Rollup.Data                  (minReq, rollupFee, datumHashEx1, evolve)
import           ZkFold.Cardano.UPLC.Common                  (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.Rollup                  (RollupInfo (..), RollupRedeemer (..))

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

    return $ RollupInfo parkingTag dataUpdate2 state2 rollupRedeemer2

rollupNext :: FilePath -> IO ()
rollupNext path = do
    let testData = path </> "test-data"
        assets   = path </> "assets"
    rollupInfoE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assets </> "rollupInfo.json")

    let rollupInfoScriptData = case rollupInfoE of
          Right a -> a
          Left _  -> error "JSON error: unreadable 'rollupInfo.json'"

    let rollupInfo = fromJust . fromData . toPlutusData . getScriptData $ rollupInfoScriptData :: RollupInfo
    IdentityCircuitContract x _ <- fromJust . decode <$> BL.readFile (testData </> "plonk-raw-contract-data.json")
    parkingTag  <- read @Integer <$> IO.readFile (assets </> "parkingTag.txt")
    newRollupInfo <- nextRollup x parkingTag rollupInfo
    BS.writeFile (assets </> "newRollupInfo.json") $ prettyPrintJSON $ dataToJSON newRollupInfo
