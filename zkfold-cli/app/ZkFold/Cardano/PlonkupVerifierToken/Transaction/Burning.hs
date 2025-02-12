module ZkFold.Cardano.PlonkupVerifierToken.Transaction.Burning (tokenBurning) where

import           Cardano.Api                              (AssetName (..), SerialiseAsRawBytes (..))
import qualified Cardano.Api                              as Api
import           Cardano.Api.Ledger                       (toCBOR)
import           Codec.CBOR.Write                         (toLazyByteString)
import qualified Codec.Serialise                          as Codec
import           Data.Aeson                               (decode)
import qualified Data.ByteString.Lazy                     as BL
import           Data.Coerce                              (coerce)
import           Data.Maybe                               (fromJust)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3                       (ToData (..), fromBuiltin)
import           PlutusTx.Builtins                        (BuiltinByteString, BuiltinData)
import           Prelude                                  (FilePath, IO, Maybe (..), Show (..), print, undefined, ($),
                                                           (++), (.), (<$>), (<>))
import           System.FilePath                          ((</>))

import           ZkFold.Cardano.Examples.EqualityCheck    (EqualityCheckContract (..), equalityCheckVerificationBytes)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F       as F
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes (..))
import           ZkFold.Cardano.UPLC.ForwardingScripts    (forwardingMintCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)



tokenNameFromApi :: Api.AssetName -> GYTokenName
tokenNameFromApi = coerce

-- | burining tokens to the reward.
burnTokens ::
  GYNetworkId ->
  GYProviders ->
  GYPaymentSigningKey ->
  GYAddress ->
  GYTxIn PlutusV3 ->
  GYTxIn PlutusV3 ->
  GYTxOutRef ->
  GYAddress ->
  GYScript 'PlutusV3 ->
  GYScript 'PlutusV3 ->
  GYTxId ->
  GYTxId ->
  BuiltinData ->
  AssetName ->
  BuiltinByteString ->
  IO ()
burnTokens nid providers skey changeAddr txIn1 txIn2 forwardingMintIn ownAddr plonkupVerifierToken forwardingMint txidSetup txidForward redeemer' assetName datum = do
    pkh <- addressToPubKeyHashIO changeAddr
    let w1 = User' skey Nothing changeAddr

        txOutRefSetup = txOutRefFromTuple (txidSetup, 0)
        setupRef = GYMintReference @PlutusV3 txOutRefSetup $ validatorToScript plonkupVerifierToken

        forwardingMintRef = txOutRefFromTuple (txidForward, 0)
        forwardRef = GYBuildPlutusScriptReference @PlutusV3 forwardingMintRef (validatorToScript forwardingMint)

        inlineDatum = Just $ datumFromPlutusData datum
        witness = GYTxInWitnessScript forwardRef inlineDatum unitRedeemer

        dummyRedeemer = redeemerFromPlutus' redeemer'
        tokenName = tokenNameFromApi assetName

    let reward = valueFromLovelace 10000000

    -- --tx-in-collateral $collateral
    let skeleton = mustHaveInput txIn1
                <> mustHaveInput txIn2
                <> mustHaveInput (GYTxIn @PlutusV3 forwardingMintIn witness)
                <> mustHaveOutput (GYTxOut ownAddr reward Nothing Nothing)
                <> mustMint setupRef dummyRedeemer tokenName (-1)
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    print $ "transaction id: " ++ show txid

tokenBurning :: FilePath -> IO ()
tokenBurning path = do
    let testData = path </> "test-data"

    let fmLabel = 0  -- Use a different label (number) to get another 'forwardingMint' address

    let nid = undefined
        providers = undefined
        skey = undefined
        changeAddr = undefined
        txIn1 = undefined
        txIn2 = undefined
        forwardingMintIn = undefined
        ownAddr = undefined
        txidSetup = undefined
        txidForward = undefined
        setup = undefined
        plonkupVerifierToken = validatorFromPlutus $ plonkupVerifierTokenCompiled setup
        forwardingMint       = validatorFromPlutus $ forwardingMintCompiled fmLabel
        policyid = mintingPolicyIdToApi $ mintingPolicyId forwardingMint
        datum = Codec.deserialise $ toLazyByteString $ toCBOR $ serialiseToRawBytes policyid

    EqualityCheckContract{..} <- fromJust . decode <$> BL.readFile (testData </> "plonkup-raw-contract-data.json")

    let (_, input, _) = equalityCheckVerificationBytes x ps targetValue
        assetName = AssetName $ fromBuiltin $ F.fromInput input
        redeemer = toBuiltinData $ ProofBytes "" "" "" "" "" "" "" "" "" "" "" "" "" 0 0 0 0 0 0 0 0 0 0 0 0 (F.F 0)

    burnTokens nid providers skey changeAddr txIn1 txIn2 forwardingMintIn ownAddr plonkupVerifierToken forwardingMint txidSetup txidForward redeemer assetName datum

