module PlonkupVerifierToken.Transaction.Minting (sendMintTokens) where

import           Cardano.Api                    (AssetName)
import qualified Cardano.Api                    as Api
import           Data.Coerce                    (coerce)
import qualified Data.Map.Strict                as Map
import           GeniusYield.Transaction.Common (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types              (GYAddress, GYAssetClass (..), GYNetworkId, GYPaymentSigningKey,
                                                 GYProviders, GYScript, GYTokenName (..), GYTxId, GYTxIn, GYTxOut (..),
                                                 PlutusVersion (..), gyGetProtocolParameters, redeemerFromPlutus',
                                                 txOutRefFromTuple, valueFromLovelace, valueMake)
import           GeniusYield.Types.BuildScript
import           GeniusYield.Types.Script       (mintingPolicyId, validatorToScript)
import           PlutusTx.Builtins              (BuiltinData)
import           Prelude                        (IO, Maybe (..), Show (..), print, toInteger, ($), (++), (<>))

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
