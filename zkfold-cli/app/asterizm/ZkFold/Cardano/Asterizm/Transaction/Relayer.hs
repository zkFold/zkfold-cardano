module ZkFold.Cardano.Asterizm.Transaction.Relayer where

import qualified Data.ByteString               as BS
import           Data.Maybe                    (fromJust)
import           GeniusYield.GYConfig          (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3            as V3
import           Prelude

import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus, submitTxWithCborOnFailure)
import           ZkFold.Cardano.Options.Common (readPaymentVerificationKey)
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmRelayerCompiled)


data Transaction = Transaction
  { coreCfgFile     :: !FilePath
  , signingKeyFile  :: !FilePath
  , relayerVKeyFile :: !FilePath
  , outAddress      :: !GYAddress
  , messageHash     :: !BS.ByteString
  }

relayerMint :: Transaction -> IO ()
relayerMint (Transaction cfgFile skeyFile relayerVkeyFile sendTo msgHash) = do
  coreCfg     <- coreConfigIO cfgFile
  skey        <- readPaymentSigningKey skeyFile
  relayerVkey <- readPaymentVerificationKey relayerVkeyFile

  let nid = cfgNetworkId coreCfg

  let signerPkh = pubKeyHash $ paymentVerificationKey skey
      changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash signerPkh
      w1         = User' skey Nothing changeAddr

  let relayerPkh = pubKeyHash relayerVkey
      redeemer = redeemerFromPlutusData $ toBuiltin msgHash

  let plutusPolicy       = asterizmRelayerCompiled $ pubKeyHashToPlutus relayerPkh
      (policy, policyId) = policyFromPlutus plutusPolicy

  let tokenName  = fromJust $ tokenNameFromBS msgHash
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let skeleton = mustHaveOutput (GYTxOut sendTo tokenValue Nothing Nothing)
              <> mustMint policy redeemer tokenName 1
              <> mustBeSignedBy relayerPkh

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    txbody <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1
                                 (buildTxBody skeleton)

    tx <- runGYTxGameMonadIO nid
                              providers $
                              asUser w1
                              (signTxBody txbody)

    txid <- submitTxWithCborOnFailure nid providers w1 tx

    print txid
