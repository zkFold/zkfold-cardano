module ZkFold.Cardano.Asterizm.Transaction.Client where

import           Control.Exception             (throwIO)
import           Control.Monad                 (forM)
import qualified Data.ByteString               as BS
import           Data.Maybe                    (fromJust)
import           GeniusYield.GYConfig          (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3            as V3
import           Prelude

import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import           ZkFold.Cardano.Options.Common (readPaymentVerificationKey)
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmClientCompiled, asterizmRelayerCompiled, buildCrosschainHash)


-- | Transaction for sending an outgoing cross-chain message.
data SendTransaction = SendTransaction
  { stCoreCfgFile      :: !FilePath
  , stSigningKeyFile   :: !FilePath
  , stClientVKeyFile   :: !FilePath
  , stOutAddress       :: !GYAddress
  , stMessage          :: !BS.ByteString
  }

-- | Transaction for receiving an incoming cross-chain message.
data ReceiveTransaction = ReceiveTransaction
  { rtCoreCfgFile       :: !FilePath
  , rtSigningKeyFile    :: !FilePath
  , rtClientVKeyFile    :: !FilePath
  , rtRelayerVKeyFiles  :: ![FilePath]
  , rtOutAddress        :: !GYAddress
  , rtMessage           :: !BS.ByteString
  }

-- | Mint client token for outgoing message (no relayer verification).
clientSend :: SendTransaction -> IO ()
clientSend (SendTransaction cfgFile skeyFile clientVkeyFile sendTo msg) = do
  coreCfg    <- coreConfigIO cfgFile
  skey       <- readPaymentSigningKey skeyFile
  clientVkey <- readPaymentVerificationKey clientVkeyFile

  let nid = cfgNetworkId coreCfg

  let signerPkh = pubKeyHash $ paymentVerificationKey skey
      changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash signerPkh
      w1         = User' skey Nothing changeAddr

  let clientPKH        = pubKeyHashToPlutus $ pubKeyHash clientVkey
      allowedRelayers  = []  -- Empty for outgoing
      isIncoming       = False
      plutusPolicy     = asterizmClientCompiled clientPKH allowedRelayers isIncoming
      (policy, policyId) = policyFromPlutus plutusPolicy

  let msgHash    = fromBuiltin . buildCrosschainHash . toBuiltin $ msg
      tokenName  = fromJust $ tokenNameFromBS msgHash
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let inlineDatum = Just (datumFromPlutusData (toBuiltin msg), GYTxOutUseInlineDatum @PlutusV3)

  let skeleton = mustHaveOutput (GYTxOut sendTo tokenValue inlineDatum Nothing)
              <> mustMint policy unitRedeemer tokenName 1
              <> mustBeSignedBy (pubKeyHash clientVkey)

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

-- | Mint client token for incoming message (requires relayer verification).
clientReceive :: ReceiveTransaction -> IO ()
clientReceive (ReceiveTransaction cfgFile skeyFile clientVkeyFile relayerVkeyFiles sendTo msg) = do
  coreCfg      <- coreConfigIO cfgFile
  skey         <- readPaymentSigningKey skeyFile
  clientVkey   <- readPaymentVerificationKey clientVkeyFile
  relayerVkeys <- mapM readPaymentVerificationKey relayerVkeyFiles

  let nid = cfgNetworkId coreCfg

  let signerPkh = pubKeyHash $ paymentVerificationKey skey
      changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash signerPkh
      w1         = User' skey Nothing changeAddr

  -- Derive relayer policy IDs from their verification keys
  let relayerPolicyIds = fmap (snd . policyFromPlutus . asterizmRelayerCompiled . pubKeyHashToPlutus . pubKeyHash) relayerVkeys
      relayerCSs       = mintingPolicyIdToCurrencySymbol <$> relayerPolicyIds

  let clientPKH        = pubKeyHashToPlutus $ pubKeyHash clientVkey
      allowedRelayers  = relayerCSs
      isIncoming       = True
      plutusPolicy     = asterizmClientCompiled clientPKH allowedRelayers isIncoming
      (policy, policyId) = policyFromPlutus plutusPolicy

  let msgHash    = fromBuiltin . buildCrosschainHash . toBuiltin $ msg
      tokenName  = fromJust $ tokenNameFromBS msgHash
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let inlineDatum = Just (datumFromPlutusData (toBuiltin msg), GYTxOutUseInlineDatum @PlutusV3)

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    -- Find relayer's token as reference input
    relayerTokens <- case mapM mintingPolicyIdFromCurrencySymbol relayerCSs of
      Right pids -> pure $ (`GYNonAdaToken` tokenName) <$> pids
      Left _     -> throwIO $ userError "Corrupted relayers' registry."

    relayerUtxos <- forM relayerTokens $ runGYTxQueryMonadIO nid providers . utxosWithAsset

    relayerOref <- case concatMap utxosToList relayerUtxos of
      u : _ -> pure $ utxoRef u
      _     -> throwIO $ userError "No relayer has validated client's message yet."

    let skeleton = mustHaveRefInput relayerOref
                <> mustHaveOutput (GYTxOut sendTo tokenValue inlineDatum Nothing)
                <> mustMint policy unitRedeemer tokenName 1
                <> mustBeSignedBy (pubKeyHash clientVkey)

    txbody <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1
                                 (buildTxBody skeleton)

    txid <- runGYTxGameMonadIO nid
                               providers $
                               asUser w1
                               (signTxBody txbody >>= submitTx)

    print txid
