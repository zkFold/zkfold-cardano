module PlonkupVerifierToken.Transaction.Transfer (sendDatum) where

import           GeniusYield.Transaction.Common (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types              (GYAddress, GYNetworkId, GYPaymentSigningKey, GYProviders, GYScript,
                                                 GYTxIn, GYTxOut (..), GYTxOutUseInlineDatum (..), PlutusVersion (..),
                                                 addressFromValidator, datumFromPlutusData, gyGetProtocolParameters,
                                                 valueFromLovelace)
import           PlutusTx.Builtins              (BuiltinByteString)
import           Prelude                        (IO, Maybe (..), Show (..), print, toInteger, ($), (++), (<>))

-- | Sending a datum script to the network.
sendDatum ::
  GYNetworkId ->
  GYProviders ->
  GYPaymentSigningKey ->
  GYAddress ->
  GYTxIn PlutusV3 ->
  GYScript 'PlutusV3 ->
  BuiltinByteString ->
  IO ()
sendDatum nid providers skey changeAddr txIn validator datum = do
    pkh <- addressToPubKeyHashIO changeAddr
    let w1 = User' skey Nothing changeAddr
        inlineDatum = Just (datumFromPlutusData datum, GYTxOutUseInlineDatum @PlutusV3)
        validdatorAddr = addressFromValidator nid validator
        outMin = GYTxOut validdatorAddr (valueFromLovelace 0) inlineDatum Nothing

    params <- gyGetProtocolParameters providers
    let calculateMin = valueFromLovelace $ toInteger $ minimumUTxO params outMin

    let skeleton = mustHaveInput txIn
                <> mustHaveOutput (GYTxOut validdatorAddr calculateMin inlineDatum Nothing)
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    print $ "transaction id: " ++ show txid





