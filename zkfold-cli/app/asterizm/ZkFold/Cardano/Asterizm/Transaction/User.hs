module ZkFold.Cardano.Asterizm.Transaction.User where

import qualified Data.ByteString               as BS
import           Data.Maybe                    (fromJust)
import           GeniusYield.GYConfig          (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3            as V3
import           Prelude

import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmUserCompiled, buildCrosschainHash)


-- | Transaction for sending a user outgoing cross-chain message.
-- Unlike 'Client.SendTransaction', no client verification key is needed
-- because the on-chain script does not verify any signature.
data SendTransaction = SendTransaction
  { ustCoreCfgFile    :: !FilePath
  , ustSigningKeyFile :: !FilePath
  , ustOutAddress     :: !GYAddress
  , ustMessage        :: !BS.ByteString
  }

-- | Mint user token for outgoing message.
-- The minting policy is universal (not parameterized by any PKH)
-- and only validates that the token name matches the cross-chain hash.
userSend :: SendTransaction -> IO ()
userSend (SendTransaction cfgFile skeyFile sendTo msg) = do
  coreCfg <- coreConfigIO cfgFile
  skey    <- readPaymentSigningKey skeyFile

  let nid = cfgNetworkId coreCfg

  let signerPkh = pubKeyHash $ paymentVerificationKey skey
      changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash signerPkh
      w1         = User' skey Nothing changeAddr

  let plutusPolicy       = asterizmUserCompiled
      (policy, policyId) = policyFromPlutus plutusPolicy

  let msgHash    = fromBuiltin . buildCrosschainHash . toBuiltin $ msg
      tokenName  = fromJust $ tokenNameFromBS msgHash
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let inlineDatum = Just (datumFromPlutusData (toBuiltin msg), GYTxOutUseInlineDatum @PlutusV3)

  let skeleton = mustHaveOutput (GYTxOut sendTo tokenValue inlineDatum Nothing)
              <> mustMint policy unitRedeemer tokenName 1

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    txbody <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1
                                 (buildTxBody skeleton)

    txid <- runGYTxGameMonadIO nid
                               providers $
                               asUser w1
                               (signTxBody txbody >>= submitTx)

    print txid
