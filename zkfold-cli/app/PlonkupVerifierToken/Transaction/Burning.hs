module PlonkupVerifierToken.Transaction.Burning (burnTokens) where

import           Cardano.Api                    (AssetName)
import qualified Cardano.Api                    as Api
import           Data.Coerce                    (coerce)
import           GeniusYield.TxBuilder
import           GeniusYield.Types             
import           PlutusTx.Builtins              (BuiltinData, BuiltinByteString)
import           Prelude                        (IO, Maybe (..), Show (..), print, ($), (++), (<>))

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
