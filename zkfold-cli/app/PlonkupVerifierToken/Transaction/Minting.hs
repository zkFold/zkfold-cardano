module PlonkupVerifierToken.Transaction.Minting (tokenMinting) where

import           Cardano.Api                              (AssetName (..))
import qualified Cardano.Api                              as Api
import           Data.Aeson                               (decode)
import qualified Data.ByteString.Lazy                     as BL
import           Data.Coerce                              (coerce)
import qualified Data.Map.Strict                          as Map
import           Data.Maybe                               (fromJust)
import           GeniusYield.Transaction.Common           (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types                        (GYAddress, GYAssetClass (..), GYNetworkId,
                                                           GYPaymentSigningKey, GYProviders, GYScript, GYTokenName (..),
                                                           GYTxId, GYTxIn, GYTxOut (..), PlutusVersion (..),
                                                           gyGetProtocolParameters, redeemerFromPlutus',
                                                           txOutRefFromTuple, valueFromLovelace, valueMake)
import           GeniusYield.Types.BuildScript
import           GeniusYield.Types.Script                 (mintingPolicyId, validatorFromPlutus, validatorToScript)
import           PlutusLedgerApi.V3                       (ToData (..), fromBuiltin)
import           PlutusTx.Builtins                        (BuiltinData)
import           Prelude                                  (FilePath, IO, Maybe (..), Show (..), print, toInteger,
                                                           undefined, ($), (++), (.), (<$>), (<>))
import           System.FilePath                          ((</>))

import           ZkFold.Cardano.Examples.EqualityCheck    (EqualityCheckContract (..), equalityCheckVerificationBytes)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F       as F
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)



tokenNameFromApi :: Api.AssetName -> GYTokenName
tokenNameFromApi = coerce

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
  IO ()
sendMintTokens nid providers skey changeAddr txIn sendTo validator txidSetup redeemer' assetName = do
    pkh <- addressToPubKeyHashIO changeAddr
    let w1 = User' skey Nothing changeAddr
        txOutRefSetup = txOutRefFromTuple (txidSetup, 0)
        redeemer = redeemerFromPlutus' redeemer'
        tokenName = tokenNameFromApi assetName
        refScript = GYMintReference @PlutusV3 txOutRefSetup $ validatorToScript validator
        tokens = valueMake $ Map.singleton (GYToken (mintingPolicyId validator) tokenName) 1
        outMin = GYTxOut sendTo tokens Nothing Nothing

    params <- gyGetProtocolParameters providers
    let calculateMin = valueFromLovelace $ toInteger $ minimumUTxO params outMin

    -- --tx-in-collateral $collateral
    let skeleton = mustHaveInput txIn
                <> mustHaveOutput (GYTxOut sendTo (calculateMin <> tokens) Nothing Nothing)
                <> mustMint refScript redeemer tokenName 1
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    print $ "transaction id: " ++ show txid

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
