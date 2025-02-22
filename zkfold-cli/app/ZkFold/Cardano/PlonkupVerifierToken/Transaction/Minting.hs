module ZkFold.Cardano.PlonkupVerifierToken.Transaction.Minting (tokenMinting, Transaction(..)) where

import           Cardano.Api                              (AddressAny, AssetName (..), TxIn)
import           Cardano.CLI.Read                         (SomeSigningWitness (..), readWitnessSigningData)
import           Cardano.CLI.Types.Common                 (WitnessSigningData)
import           Data.Aeson                               (decode, decodeFileStrict, encodeFile)
import qualified Data.ByteString.Lazy                     as BL
import           Data.Coerce                              (coerce)
import qualified Data.Map.Strict                          as Map
import           Data.Maybe                               (fromJust)
import           GeniusYield.GYConfig                     (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.Transaction.Common           (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3                       (ToData (..), fromBuiltin)
import           PlutusTx.Builtins                        (BuiltinData)
import           Prelude                                  (Either (..), FilePath, IO, Maybe (..), toInteger, ($), (.),
                                                           (<$>), (<>))
import           System.FilePath                          ((</>))

import           ZkFold.Cardano.Examples.EqualityCheck    (EqualityCheckContract (..), equalityCheckVerificationBytes)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F       as F
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)

data Transaction = Transaction
    { curPath         :: !FilePath
    , pathCoreCfg     :: !FilePath
    , txIn            :: !TxIn
    , requiredSigners :: !WitnessSigningData
    , changeAddresses :: !AddressAny
    , outAddress      :: !AddressAny
    , txidSetupFile   :: !FilePath
    , outFile         :: !FilePath
    }

-- | Sending a tokens script to the address.
sendMintTokens ::
    GYNetworkId ->
    GYProviders ->
    GYPaymentSigningKey ->
    GYAddress ->
    GYTxIn PlutusV3 ->
    GYAddress ->
    GYScript 'PlutusV3 ->
    GYTxId ->
    BuiltinData ->
    AssetName ->
    FilePath ->
    IO ()
sendMintTokens nid providers skey changeAddr txIn sendTo validator txidSetup redeemer' assetName outFile = do
    let w1 = User' skey Nothing changeAddr
        txOutRefSetup = txOutRefFromTuple (txidSetup, 0)
        redeemer = redeemerFromPlutus' redeemer'
        tokenName = coerce assetName
        refScript = GYMintReference @PlutusV3 txOutRefSetup $ validatorToScript validator
        tokens = valueMake $ Map.singleton (GYToken (mintingPolicyId validator) tokenName) 1
        outMin = GYTxOut sendTo tokens Nothing Nothing

    params <- gyGetProtocolParameters providers
    let calculateMin = valueFromLovelace $ toInteger $ minimumUTxO params outMin

    -- --tx-in-collateral $collateral
    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = mustHaveInput txIn
                <> mustHaveOutput (GYTxOut sendTo (calculateMin <> tokens) Nothing Nothing)
                <> mustMint refScript redeemer tokenName 1
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    encodeFile outFile txid

tokenMinting :: Transaction -> IO ()
tokenMinting (Transaction path pathCfg txIn sig changeAddr outAddress txidSetupFile outFile) = do
    let testData = path </> "test-data"
    EqualityCheckContract{..} <- fromJust . decode <$> BL.readFile (testData </> "plonkup-raw-contract-data.json")

    let (setup, input, proof) = equalityCheckVerificationBytes x ps targetValue
        assetName = AssetName $ fromBuiltin $ F.fromInput input
        redeemer  = toBuiltinData proof

    coreCfg <- coreConfigIO pathCfg
    (Right (APaymentSigningWitness sks)) <- readWitnessSigningData sig
    (Just txId) <- decodeFileStrict txidSetupFile

    let nid         = cfgNetworkId coreCfg
        skey        = signingKeyFromApi sks
        changeAddr' = addressFromApi changeAddr
        txIn'       = GYTxIn (txOutRefFromApi txIn) GYTxInWitnessKey
        sendTo      = addressFromApi outAddress

        plonkupVerifierToken = validatorFromPlutus $ plonkupVerifierTokenCompiled setup

    withCfgProviders coreCfg "main" $ \providers -> do
       sendMintTokens nid providers skey changeAddr' txIn' sendTo plonkupVerifierToken txId redeemer assetName outFile
