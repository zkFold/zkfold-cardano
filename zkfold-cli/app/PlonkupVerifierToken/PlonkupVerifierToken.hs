module PlonkupVerifierToken.PlonkupVerifierToken where

import           Cardano.Api                               (AssetName (..), SerialiseAsRawBytes (..))
import           Cardano.Api.Ledger                        (toCBOR)
import           Codec.CBOR.Write                          (toLazyByteString)
import qualified Codec.Serialise                           as Codec
import           Data.Aeson                                (decode, encode)
import qualified Data.ByteString.Lazy                      as BL
import           Data.Maybe                                (fromJust)
import           GeniusYield.Types.Script                  (mintingPolicyId, mintingPolicyIdToApi, validatorFromPlutus)
import           PlonkupVerifierToken.Transaction.Init     (sendScript)
import           PlonkupVerifierToken.Transaction.Minting  (sendMintTokens)
import           PlonkupVerifierToken.Transaction.Transfer (sendDatum)
import           PlutusLedgerApi.V3                        (ToData (..), fromBuiltin)
import           Prelude                                   (Bool (..), FilePath, IO, undefined, ($), (.), (<$>))
import           System.Directory                          (createDirectoryIfMissing)
import           System.FilePath                           ((</>))
import           Test.QuickCheck.Arbitrary                 (Arbitrary (..))
import           Test.QuickCheck.Gen                       (generate)

import           ZkFold.Cardano.Examples.EqualityCheck     (EqualityCheckContract (..), equalityCheckVerificationBytes)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F        as F
import           ZkFold.Cardano.OnChain.Plonkup.Data       (ProofBytes (..))
import           ZkFold.Cardano.UPLC.ForwardingScripts     (forwardingMintCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken  (plonkupVerifierTokenCompiled)

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

    let nid = undefined
        providers = undefined
        skey = undefined
        changeAddr = undefined
        txIn = undefined
        sendTo = undefined
        plonkupVerifierToken = validatorFromPlutus $ plonkupVerifierTokenCompiled setup
        forwardingMint       = validatorFromPlutus $ forwardingMintCompiled fmLabel

    sendScript nid providers skey changeAddr txIn sendTo plonkupVerifierToken "plonkupVerifierToken"
    sendScript nid providers skey changeAddr txIn sendTo forwardingMint "forwardingMint"

dummyRedeemer :: ProofBytes
dummyRedeemer = ProofBytes e e e e e e e e e e e e e 0 0 0 0 0 0 0 0 0 0 0 0 (F.F 0)
    where e = ""

tokenMinting :: FilePath -> IO ()
tokenMinting path = do
    let testData = path </> "test-data"

    let nid = undefined
        providers = undefined
        skey = undefined
        changeAddr = undefined
        txIn = undefined
        sendTo = undefined
        txidSetup = undefined
        setup = undefined
        plonkupVerifierToken = validatorFromPlutus $ plonkupVerifierTokenCompiled setup

    EqualityCheckContract{..} <- fromJust . decode <$> BL.readFile (testData </> "plonkup-raw-contract-data.json")

    let (_, input, proof) = equalityCheckVerificationBytes x ps targetValue
        assetName = AssetName $ fromBuiltin $ F.fromInput input
        redeemer = toBuiltinData proof

    sendMintTokens nid providers skey changeAddr txIn sendTo plonkupVerifierToken txidSetup redeemer assetName

{-
unitDatum :: GYDatum
unitDatum = datumFromPlutusData ()
-- datumFromApi' :: Api.HashableScriptData -> GYDatum
-- unsafeHashableScriptData :: ScriptData -> HashableScriptData
-}

tokenTransfer :: IO ()
tokenTransfer = do
    let nid = undefined
        providers = undefined
        skey = undefined
        changeAddr = undefined
        txIn = undefined
        fmLabel = 0
        forwardingMint = validatorFromPlutus $ forwardingMintCompiled fmLabel
        policyid = mintingPolicyIdToApi $ mintingPolicyId forwardingMint

    let datum = Codec.deserialise $ toLazyByteString $ toCBOR $ serialiseToRawBytes policyid

    sendDatum nid providers skey changeAddr txIn forwardingMint datum
