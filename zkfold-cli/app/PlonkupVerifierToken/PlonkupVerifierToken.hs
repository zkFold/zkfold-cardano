module PlonkupVerifierToken.PlonkupVerifierToken where

import           Cardano.Api                              (AssetName (..), SerialiseAsRawBytes (..),
                                                           UsingRawBytesHex (..), policyId)
import           Cardano.Api.Ledger                       (toCBOR)
import           Codec.CBOR.Write                         (toStrictByteString)
import           Data.Aeson                               (decode, encode)
import           Data.ByteString                          as BS (writeFile)
import qualified Data.ByteString.Lazy                     as BL
import           Data.Maybe                               (fromJust)
import           Data.String                              (IsString (..))
import           GeniusYield.Types.Script
import           PlonkupVerifierToken.Transaction.Init
import           PlutusLedgerApi.V3                       (fromBuiltin)
import           Prelude                                  (Bool (..), Either (..), FilePath, IO, Show (..), String,
                                                           error, head, undefined, ($), (++), (.), (<$>))
import           System.Directory                         (createDirectoryIfMissing)
import           System.FilePath                          ((</>))
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)
import           Text.Parsec                              (parse)

import           ZkFold.Cardano.Examples.EqualityCheck    (EqualityCheckContract (..), equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils            (dataToCBOR)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F       as F
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes (..))
import           ZkFold.Cardano.UPLC.ForwardingScripts    (forwardingMintCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)

tokenInit :: FilePath -> IO ()
tokenInit path = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary

    let contract = EqualityCheckContract x ps targetValue

    let testData = path </> "test-data"
        assets   = path </> "assets"

    createDirectoryIfMissing True testData
    createDirectoryIfMissing True assets

    BL.writeFile (testData </> "plonkup-raw-contract-data.json") $ encode contract

    let (setup, _, _) = equalityCheckVerificationBytes x ps targetValue

    let fmLabel = 0  -- Use a different label (number) to get another 'forwardingMint' address

    let ctx  = Ctx undefined undefined
        skey = undefined
        changeAddr = undefined
        txIn = undefined
        sendTo = undefined
        plonkupVerifierToken = validatorFromPlutus $ plonkupVerifierTokenCompiled setup
        forwardingMint       = validatorFromPlutus $ forwardingMintCompiled fmLabel

    sendScript ctx skey changeAddr txIn sendTo plonkupVerifierToken "plonkupVerifierToken"
    sendScript ctx skey changeAddr txIn sendTo forwardingMint "forwardingMint"

dummyRedeemer :: ProofBytes
dummyRedeemer = ProofBytes e e e e e e e e e e e e e 0 0 0 0 0 0 0 0 0 0 0 0 (F.F 0)
    where e = ""

tokenMinting :: FilePath -> IO ()
tokenMinting path = do
    let testData = path </> "test-data"
        assets   = path </> "assets"

    EqualityCheckContract{..} <- fromJust . decode <$> BL.readFile (testData </> "plonkup-raw-contract-data.json")

    let (_, input, proof) = equalityCheckVerificationBytes x ps targetValue

    BS.writeFile (assets </> "tokenname") $ fromString $ show $ UsingRawBytesHex $ AssetName $ fromBuiltin $ F.fromInput input
    BS.writeFile (assets </> "unit.cbor") $ dataToCBOR ()
    BS.writeFile (assets </> "redeemerPlonkupVerifierToken.cbor") $ dataToCBOR proof
    BS.writeFile (assets </> "dummy-redeemer.cbor") $ dataToCBOR dummyRedeemer

tokenTransfer :: FilePath -> [String] -> IO ()
tokenTransfer path args = do
    let assets   = path </> "assets"

    let policyidE = parse policyId "" (head args)

    let policyid = case policyidE of
          Right a  -> a
          Left err -> error $ "parse" ++ show err -- fail

    BS.writeFile (assets </> "datumPlonkupVerifierToken.cbor") $ toStrictByteString $ toCBOR $ serialiseToRawBytes policyid
