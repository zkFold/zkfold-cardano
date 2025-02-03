module PlonkupVerifierToken.Transaction.Init (sendScript) where


import           GeniusYield.Transaction.Common (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types              (GYAddress, GYAnyScript (..), GYNetworkId, GYPaymentSigningKey,
                                                 GYProviders, GYScript, GYTxIn, GYTxOut (..), PlutusVersion (..),
                                                 gyGetProtocolParameters, valueFromLovelace)
import           Prelude                        (IO, Maybe (..), Show (..), String, print, toInteger, ($), (++), (<>))

type ScriptName = String

-- | Sending a compiled script to the network.
sendScript ::
  GYNetworkId ->
  GYProviders ->
  GYPaymentSigningKey ->
  GYAddress ->
  GYTxIn PlutusV3 ->
  GYAddress ->
  GYScript 'PlutusV3 ->
  ScriptName ->
  IO ()
sendScript nid providers skey changeAddr txIn sendTo validator name = do
    pkh <- addressToPubKeyHashIO changeAddr
    let w1 = User' skey Nothing changeAddr
        validator' = Just $ GYPlutusScript validator
        outMin = GYTxOut sendTo (valueFromLovelace 0) Nothing validator'

    params <- gyGetProtocolParameters providers
    let calculateMin = valueFromLovelace $ toInteger $ minimumUTxO params outMin

    let skeleton = mustHaveInput txIn
                <> mustHaveOutput (GYTxOut sendTo calculateMin Nothing validator')
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    print $ name ++ " transaction id: " ++ show txid
