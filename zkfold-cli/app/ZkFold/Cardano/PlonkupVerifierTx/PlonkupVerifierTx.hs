module ZkFold.Cardano.PlonkupVerifierTx.PlonkupVerifierTx where

import           Cardano.Api                           (SerialiseAsRawBytes (..), policyId, prettyPrintJSON)
import           Cardano.Api.Ledger                    (toCBOR)
import           Codec.CBOR.Write                      (toStrictByteString)
import           Data.Aeson                            (decode, encode)
import           Data.ByteString                       as BS (writeFile)
import qualified Data.ByteString.Lazy                  as BL
import           Data.Maybe                            (fromJust)
import           Prelude                               (Bool (..), Either (..), FilePath, IO, Show (..), String, error,
                                                        head, ($), (++), (.), (<$>))
import           System.Directory                      (createDirectoryIfMissing)
import           System.FilePath                       ((</>))
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)
import           Text.Parsec                           (parse)

import           ZkFold.Cardano.Examples.EqualityCheck (EqualityCheckContract (..), equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils         (dataToJSON, savePlutus)
import           ZkFold.Cardano.UPLC.ForwardingScripts (forwardingRewardCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx (plonkupVerifierTxCompiled)

txInit :: FilePath -> IO ()
txInit path = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary

    let contract = EqualityCheckContract x ps targetValue

    let testData = path </> "test-data"
        assets   = path </> "assets"

    createDirectoryIfMissing True testData
    createDirectoryIfMissing True assets

    BL.writeFile (testData </> "plonkupVerifierTx-raw-contract-data.json") $ encode contract

    let (setup, _, _) = equalityCheckVerificationBytes x ps targetValue

    savePlutus (assets </> "plonkupVerifierTx.plutus") $ plonkupVerifierTxCompiled setup
    savePlutus (assets </> "forwardingReward.plutus") forwardingRewardCompiled

txTransfer :: FilePath -> [String] -> IO ()
txTransfer path args = do
    let policyidE = parse policyId "" (head args)

    let policyid = case policyidE of
            Right a  -> a
            Left err -> error $ "parse" ++ show err

    let assets   = path </> "assets"

    BS.writeFile (assets </> "datumPlonkupVerifierTx.cbor") $ toStrictByteString $ toCBOR $ serialiseToRawBytes policyid

txWithdraw :: FilePath -> IO ()
txWithdraw path = do
    let testData = path </> "test-data"
        assets   = path </> "assets"

    EqualityCheckContract{..} <- fromJust . decode <$> BL.readFile (testData </> "plonkupVerifierTx-raw-contract-data.json")

    let (_, _, proof) = equalityCheckVerificationBytes x ps targetValue

    BS.writeFile (assets </> "unit.json") $ prettyPrintJSON $ dataToJSON ()
    BS.writeFile (assets </> "redeemerPlonkupVerifierTx.json") $ prettyPrintJSON $ dataToJSON proof
