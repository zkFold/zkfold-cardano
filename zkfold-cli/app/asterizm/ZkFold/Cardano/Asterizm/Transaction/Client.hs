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

import           ZkFold.Cardano.Asterizm.Utils (clientActionRedeemer, hashMessage, hashModeRedeemer,
                                                omniTokenNameGY, paymentUserWithCollateral, policyFromPlutus,
                                                submitTxWithCborOnFailure, tokenTransferAmount)
import           ZkFold.Cardano.Options.Common (readPaymentVerificationKey)
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmClientCompiled, asterizmRelayerCompiled, asterizmUserCompiled,
                                                asterizmOmniTokenCompiled, AsterizmHashMode,
                                                AsterizmClientAction (..), AsterizmOmniTokenAction (..))


-- | Transaction for sending an outgoing cross-chain message.
data SendTransaction = SendTransaction
  { stCoreCfgFile      :: !FilePath
  , stSigningKeyFile   :: !FilePath
  , stClientVKeyFile   :: !FilePath
  , stRelayerVKeyFiles :: ![FilePath]
  , stTrustedAddresses :: ![BS.ByteString]
  , stOutAddress       :: !GYAddress
  , stHashMode         :: !AsterizmHashMode
  , stMessage          :: !BS.ByteString
  }

-- | Transaction for receiving an incoming cross-chain message.
data ReceiveTransaction = ReceiveTransaction
  { rtCoreCfgFile      :: !FilePath
  , rtSigningKeyFile   :: !FilePath
  , rtClientVKeyFile   :: !FilePath
  , rtRelayerVKeyFiles :: ![FilePath]
  , rtTrustedAddresses :: ![BS.ByteString]
  , rtOutAddress       :: !GYAddress
  , rtHashMode         :: !AsterizmHashMode
  , rtMessage          :: !BS.ByteString
  }

-- | Transaction for receiving an incoming token transfer and minting omni-chain tokens.
data TokenMintTransaction = TokenMintTransaction
  { tmtCoreCfgFile      :: !FilePath
  , tmtSigningKeyFile   :: !FilePath
  , tmtClientVKeyFile   :: !FilePath
  , tmtRelayerVKeyFiles :: ![FilePath]
  , tmtTrustedAddresses :: ![BS.ByteString]
  , tmtOutAddress       :: !GYAddress
  , tmtHashMode         :: !AsterizmHashMode
  , tmtMessage          :: !BS.ByteString
  }

-- | Transaction for approving an outgoing token transfer and burning omni-chain tokens.
data TokenBurnTransaction = TokenBurnTransaction
  { tbtCoreCfgFile      :: !FilePath
  , tbtSigningKeyFile   :: !FilePath
  , tbtClientVKeyFile   :: !FilePath
  , tbtRelayerVKeyFiles :: ![FilePath]
  , tbtTrustedAddresses :: ![BS.ByteString]
  , tbtOutAddress       :: !GYAddress
  , tbtHashMode         :: !AsterizmHashMode
  , tbtMessage          :: !BS.ByteString
  }

-- | Mint client token for outgoing message (no relayer verification).
clientSend :: SendTransaction -> IO ()
clientSend (SendTransaction cfgFile skeyFile clientVkeyFile relayerVkeyFiles trustedAddressBSs sendTo hashMode msg) = do
  coreCfg      <- coreConfigIO cfgFile
  skey         <- readPaymentSigningKey skeyFile
  clientVkey   <- readPaymentVerificationKey clientVkeyFile
  relayerVkeys <- mapM readPaymentVerificationKey relayerVkeyFiles

  let nid = cfgNetworkId coreCfg

  let relayerPolicyIds = fmap (snd . policyFromPlutus . asterizmRelayerCompiled . pubKeyHashToPlutus . pubKeyHash) relayerVkeys
      relayerCSs       = mintingPolicyIdToCurrencySymbol <$> relayerPolicyIds

  let clientPKH        = pubKeyHashToPlutus $ pubKeyHash clientVkey
      (userPolicy, userPolicyId) = policyFromPlutus asterizmUserCompiled
      userCS           = mintingPolicyIdToCurrencySymbol userPolicyId
      trustedAddresses = toBuiltin <$> trustedAddressBSs
      plutusPolicy     = asterizmClientCompiled clientPKH relayerCSs userCS trustedAddresses
      (policy, policyId) = policyFromPlutus plutusPolicy

  let msgHash    = hashMessage hashMode msg
      tokenName  = fromJust $ tokenNameFromBS msgHash
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let inlineDatum = Just (datumFromPlutusData (toBuiltin msg), GYTxOutUseInlineDatum @PlutusV3)

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    w1 <- paymentUserWithCollateral nid providers skey

    userUtxos <- runGYTxQueryMonadIO nid providers $ utxosWithAsset (GYNonAdaToken userPolicyId tokenName)

    userUtxo <- case utxosToList userUtxos of
      u : _ -> pure u
      _     -> throwIO $ userError "No user has posted client's outgoing message yet."

    let skeleton = mustHaveOutput (GYTxOut sendTo tokenValue inlineDatum Nothing)
                <> mustHaveInput (GYTxIn @PlutusV3 (utxoRef userUtxo) GYTxInWitnessKey)
                <> mustMint policy (clientActionRedeemer $ ClientOutgoing hashMode) tokenName 1
                <> mustMint userPolicy (hashModeRedeemer hashMode) tokenName (-1)
                <> mustBeSignedBy (pubKeyHash clientVkey)

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

-- | Mint client token for incoming message (requires relayer verification).
clientReceive :: ReceiveTransaction -> IO ()
clientReceive (ReceiveTransaction cfgFile skeyFile clientVkeyFile relayerVkeyFiles trustedAddressBSs sendTo hashMode msg) = do
  coreCfg      <- coreConfigIO cfgFile
  skey         <- readPaymentSigningKey skeyFile
  clientVkey   <- readPaymentVerificationKey clientVkeyFile
  relayerVkeys <- mapM readPaymentVerificationKey relayerVkeyFiles

  let nid = cfgNetworkId coreCfg

  -- Derive relayer policy IDs from their verification keys
  let relayerPolicyIds = fmap (snd . policyFromPlutus . asterizmRelayerCompiled . pubKeyHashToPlutus . pubKeyHash) relayerVkeys
      relayerCSs       = mintingPolicyIdToCurrencySymbol <$> relayerPolicyIds

  let clientPKH        = pubKeyHashToPlutus $ pubKeyHash clientVkey
      allowedRelayers  = relayerCSs
      userPolicyId     = snd . policyFromPlutus $ asterizmUserCompiled
      userCS           = mintingPolicyIdToCurrencySymbol userPolicyId
      trustedAddresses = toBuiltin <$> trustedAddressBSs
      plutusPolicy     = asterizmClientCompiled clientPKH allowedRelayers userCS trustedAddresses
      (policy, policyId) = policyFromPlutus plutusPolicy

  let msgHash    = hashMessage hashMode msg
      tokenName  = fromJust $ tokenNameFromBS msgHash
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let inlineDatum = Just (datumFromPlutusData (toBuiltin msg), GYTxOutUseInlineDatum @PlutusV3)

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    w1 <- paymentUserWithCollateral nid providers skey

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
                <> mustMint policy (clientActionRedeemer $ ClientIncoming hashMode) tokenName 1
                <> mustBeSignedBy (pubKeyHash clientVkey)

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

-- | Mint client proof token and omni-chain tokens for an incoming transfer.
clientTokenMint :: TokenMintTransaction -> IO ()
clientTokenMint (TokenMintTransaction cfgFile skeyFile clientVkeyFile relayerVkeyFiles trustedAddressBSs sendTo hashMode msg) = do
  coreCfg      <- coreConfigIO cfgFile
  skey         <- readPaymentSigningKey skeyFile
  clientVkey   <- readPaymentVerificationKey clientVkeyFile
  relayerVkeys <- mapM readPaymentVerificationKey relayerVkeyFiles
  amount       <- either (throwIO . userError) pure $ tokenTransferAmount msg

  let nid = cfgNetworkId coreCfg

  let relayerPolicyIds = fmap (snd . policyFromPlutus . asterizmRelayerCompiled . pubKeyHashToPlutus . pubKeyHash) relayerVkeys
      relayerCSs       = mintingPolicyIdToCurrencySymbol <$> relayerPolicyIds

  let clientPKH        = pubKeyHashToPlutus $ pubKeyHash clientVkey
      userPolicyId     = snd . policyFromPlutus $ asterizmUserCompiled
      userCS           = mintingPolicyIdToCurrencySymbol userPolicyId
      trustedAddresses = toBuiltin <$> trustedAddressBSs
      clientPolicy     = asterizmClientCompiled clientPKH relayerCSs userCS trustedAddresses
      (policy, policyId) = policyFromPlutus clientPolicy
      clientCS         = mintingPolicyIdToCurrencySymbol policyId
      (omniPolicy, omniPolicyId) = policyFromPlutus $ asterizmOmniTokenCompiled clientCS userCS

  let msgHash    = hashMessage hashMode msg
      tokenName  = fromJust $ tokenNameFromBS msgHash
      proofToken = GYToken policyId tokenName
      omniToken  = GYToken omniPolicyId omniTokenNameGY
      proofValue = valueSingleton proofToken 1
      omniValue  = valueSingleton omniToken amount

  let inlineDatum = Just (datumFromPlutusData (toBuiltin msg), GYTxOutUseInlineDatum @PlutusV3)

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    w1 <- paymentUserWithCollateral nid providers skey

    relayerTokens <- case mapM mintingPolicyIdFromCurrencySymbol relayerCSs of
      Right pids -> pure $ (`GYNonAdaToken` tokenName) <$> pids
      Left _     -> throwIO $ userError "Corrupted relayers' registry."

    relayerUtxos <- forM relayerTokens $ runGYTxQueryMonadIO nid providers . utxosWithAsset

    relayerOref <- case concatMap utxosToList relayerUtxos of
      u : _ -> pure $ utxoRef u
      _     -> throwIO $ userError "No relayer has validated client's message yet."

    let skeleton = mustHaveOutput (GYTxOut sendTo proofValue inlineDatum Nothing)
                <> mustHaveOutput (GYTxOut sendTo omniValue Nothing Nothing)
                <> mustHaveRefInput relayerOref
                <> mustMint policy (clientActionRedeemer $ ClientIncoming hashMode) tokenName 1
                <> mustMint omniPolicy (omniActionRedeemer OmniTokenMint) omniTokenNameGY amount
                <> mustBeSignedBy (pubKeyHash clientVkey)

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

-- | Mint client proof token and burn omni-chain tokens for an outgoing transfer.
clientTokenBurn :: TokenBurnTransaction -> IO ()
clientTokenBurn (TokenBurnTransaction cfgFile skeyFile clientVkeyFile relayerVkeyFiles trustedAddressBSs sendTo hashMode msg) = do
  coreCfg      <- coreConfigIO cfgFile
  skey         <- readPaymentSigningKey skeyFile
  clientVkey   <- readPaymentVerificationKey clientVkeyFile
  relayerVkeys <- mapM readPaymentVerificationKey relayerVkeyFiles
  amount       <- either (throwIO . userError) pure $ tokenTransferAmount msg

  let nid = cfgNetworkId coreCfg

  let relayerPolicyIds = fmap (snd . policyFromPlutus . asterizmRelayerCompiled . pubKeyHashToPlutus . pubKeyHash) relayerVkeys
      relayerCSs       = mintingPolicyIdToCurrencySymbol <$> relayerPolicyIds

  let clientPKH        = pubKeyHashToPlutus $ pubKeyHash clientVkey
      trustedAddresses = toBuiltin <$> trustedAddressBSs
      (userPolicy, userPolicyId) = policyFromPlutus asterizmUserCompiled
      userCS           = mintingPolicyIdToCurrencySymbol userPolicyId
      clientPolicy     = asterizmClientCompiled clientPKH relayerCSs userCS trustedAddresses
      (policy, policyId) = policyFromPlutus clientPolicy
      clientCS         = mintingPolicyIdToCurrencySymbol policyId
      (omniPolicy, omniPolicyId) = policyFromPlutus $ asterizmOmniTokenCompiled clientCS userCS

  let msgHash    = hashMessage hashMode msg
      tokenName  = fromJust $ tokenNameFromBS msgHash
      proofToken = GYToken policyId tokenName
      proofValue = valueSingleton proofToken 1
      omniToken  = GYToken omniPolicyId omniTokenNameGY

  let inlineDatum = Just (datumFromPlutusData (toBuiltin msg), GYTxOutUseInlineDatum @PlutusV3)

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    w1 <- paymentUserWithCollateral nid providers skey

    userUtxos <- runGYTxQueryMonadIO nid providers $ utxosWithAsset (GYNonAdaToken userPolicyId tokenName)

    userUtxo <- case filter (\u -> valueAssetClass (utxoValue u) omniToken >= amount) $ utxosToList userUtxos of
      u : _ -> pure u
      _     -> throwIO $ userError "No user UTxO contains both the message token and enough omni-chain tokens."

    let skeleton = mustHaveOutput (GYTxOut sendTo proofValue inlineDatum Nothing)
                <> mustHaveInput (GYTxIn @PlutusV3 (utxoRef userUtxo) GYTxInWitnessKey)
                <> mustMint policy (clientActionRedeemer $ ClientOutgoing hashMode) tokenName 1
                <> mustMint userPolicy (hashModeRedeemer hashMode) tokenName (-1)
                <> mustMint omniPolicy (omniActionRedeemer OmniTokenBurn) omniTokenNameGY (negate amount)
                <> mustBeSignedBy (pubKeyHash clientVkey)

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

omniActionRedeemer :: AsterizmOmniTokenAction -> GYRedeemer
omniActionRedeemer = redeemerFromPlutusData . toBuiltinData
