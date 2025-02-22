module ZkFold.Cardano.PlonkupVerifierToken.Transaction.Burning (tokenBurning, Transaction(..)) where

import           Cardano.Api                              (AddressAny, AssetName (..), SerialiseAsRawBytes (..), TxIn)
import           Cardano.Api.Ledger                       (toCBOR)
import           Cardano.CLI.Read                         (SomeSigningWitness (..), readWitnessSigningData)
import           Cardano.CLI.Types.Common                 (WitnessSigningData)
import           Codec.CBOR.Write                         (toLazyByteString)
import qualified Codec.Serialise                          as Codec
import           Control.Monad                            (void)
import           Data.Aeson                               (decode, decodeFileStrict)
import qualified Data.ByteString.Lazy                     as BL
import           Data.Coerce                              (coerce)
import           Data.Maybe                               (fromJust)
import           GeniusYield.GYConfig                     (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3                       (ToData (..), fromBuiltin)
import           PlutusTx.Builtins                        (BuiltinByteString, BuiltinData)
import           Prelude                                  (Either (..), FilePath, IO, Maybe (..), ($), (.), (<$>), (<>))
import           System.FilePath                          ((</>))

import           ZkFold.Cardano.Examples.EqualityCheck    (EqualityCheckContract (..), equalityCheckVerificationBytes)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F       as F
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes (..))
import           ZkFold.Cardano.UPLC.ForwardingScripts    (forwardingMintCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)

data Transaction = Transaction
    { curPath          :: !FilePath
    , pathCoreCfg      :: !FilePath
    , txIn1            :: !TxIn
    , txIn2            :: !TxIn
    , forwardingMintIn :: !TxIn
    , requiredSigners  :: !WitnessSigningData
    , changeAddresses  :: !AddressAny
    , outAddress       :: !AddressAny
    , txidSetupFile    :: !FilePath
    , txidForwardFile  :: !FilePath
    }

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
    let w1 = User' skey Nothing changeAddr

        txOutRefSetup = txOutRefFromTuple (txidSetup, 0)
        setupRef = GYMintReference @PlutusV3 txOutRefSetup $ validatorToScript plonkupVerifierToken

        forwardingMintRef = txOutRefFromTuple (txidForward, 0)
        forwardRef = GYBuildPlutusScriptReference @PlutusV3 forwardingMintRef (validatorToScript forwardingMint)

        inlineDatum = Just $ datumFromPlutusData datum
        witness = GYTxInWitnessScript forwardRef inlineDatum unitRedeemer

        dummyRedeemer = redeemerFromPlutus' redeemer'
        tokenName = coerce assetName

    let reward = valueFromLovelace 10000000

    -- --tx-in-collateral $collateral
    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = mustHaveInput txIn1
                <> mustHaveInput txIn2
                <> mustHaveInput (GYTxIn @PlutusV3 forwardingMintIn witness)
                <> mustHaveOutput (GYTxOut ownAddr reward Nothing Nothing)
                <> mustMint setupRef dummyRedeemer tokenName (-1)
                <> mustBeSignedBy pkh

    void $ runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

tokenBurning :: Transaction -> IO ()
tokenBurning (Transaction path pathCfg txIn1 txIn2 forwardingMintIn sig changeAddr outAddress txidSetupFile txidForwardFile) = do
    let testData = path </> "test-data"
    EqualityCheckContract{..} <- fromJust . decode <$> BL.readFile (testData </> "plonkup-raw-contract-data.json")

    let (setup, input, _) = equalityCheckVerificationBytes x ps targetValue
        assetName = AssetName $ fromBuiltin $ F.fromInput input
        redeemer  = toBuiltinData $ ProofBytes "" "" "" "" "" "" "" "" "" "" "" "" "" 0 0 0 0 0 0 0 0 0 0 0 0 (F.F 0)

    coreCfg <- coreConfigIO pathCfg
    (Right (APaymentSigningWitness sks)) <- readWitnessSigningData sig
    (Just txidSetup)   <- decodeFileStrict txidSetupFile
    (Just txidForward) <- decodeFileStrict txidForwardFile

    let fmLabel           = 0
        nid               = cfgNetworkId coreCfg
        skey              = signingKeyFromApi sks
        changeAddr'       = addressFromApi changeAddr
        txIn1'            = GYTxIn (txOutRefFromApi txIn1) GYTxInWitnessKey
        txIn2'            = GYTxIn (txOutRefFromApi txIn2) GYTxInWitnessKey
        forwardingMintIn' = txOutRefFromApi forwardingMintIn
        ownAddr           = addressFromApi outAddress

        plonkupVerifierToken = validatorFromPlutus $ plonkupVerifierTokenCompiled setup
        forwardingMint       = validatorFromPlutus $ forwardingMintCompiled fmLabel

        policyid = mintingPolicyIdToApi $ mintingPolicyId forwardingMint
        datum    = Codec.deserialise $ toLazyByteString $ toCBOR $ serialiseToRawBytes policyid

    withCfgProviders coreCfg "main" $ \providers -> do
        burnTokens nid providers skey changeAddr' txIn1' txIn2' forwardingMintIn' ownAddr plonkupVerifierToken forwardingMint txidSetup txidForward redeemer assetName datum
