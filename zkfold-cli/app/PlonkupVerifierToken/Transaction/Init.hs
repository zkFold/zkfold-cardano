module PlonkupVerifierToken.Transaction.Init where

import           GeniusYield.GYConfig           (GYCoreConfig (cfgNetworkId))
import           GeniusYield.Transaction.Common (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types              (GYAddress, GYAnyScript (..), GYPaymentSigningKey, GYProviders,
                                                 GYScript, GYTxIn, GYTxOut (..), PlutusVersion (..),
                                                 gyGetProtocolParameters, valueFromLovelace)
import           Prelude

-- | Our Context.
data Ctx = Ctx
  { ctxCoreCfg   :: !GYCoreConfig
  , ctxProviders :: !GYProviders
  }

type ScriptName = String

-- | Sending a compiled script to the network.
sendScript ::
  Ctx ->
  GYPaymentSigningKey ->
  GYAddress ->
  GYTxIn PlutusV3 ->
  GYAddress ->
  GYScript 'PlutusV3 ->
  ScriptName ->
  IO ()
sendScript ctx skey changeAddr txIn sendTo validator name = do
    pkh <- addressToPubKeyHashIO sendTo
    let nid       = cfgNetworkId $ ctxCoreCfg ctx
        providers = ctxProviders ctx

        w1 = User' skey Nothing changeAddr

    params <- gyGetProtocolParameters providers
    let calculateMin = valueFromLovelace $ toInteger $ minimumUTxO params undefined

        skeleton = mustHaveInput txIn
                <> mustHaveOutput (GYTxOut (userAddr w1) calculateMin Nothing (Just $ GYPlutusScript validator))
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    print $ name ++ " transaction id: " ++ show txid
