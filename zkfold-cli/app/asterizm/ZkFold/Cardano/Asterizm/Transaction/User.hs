module ZkFold.Cardano.Asterizm.Transaction.User where

import           Control.Exception             (throwIO)
import qualified Data.ByteString               as BS
import           Data.Maybe                    (fromJust)
import           GeniusYield.GYConfig          (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3            as V3
import           Prelude

import           ZkFold.Cardano.Asterizm.Utils (hashMessage, hashModeRedeemer, omniTokenNameGY, policyFromPlutus,
                                                paymentUserWithCollateral, submitTxWithCborOnFailure)
import           ZkFold.Cardano.UPLC.Asterizm  (AsterizmHashMode, asterizmUserCompiled)


-- | Transaction for sending a user outgoing cross-chain message.
-- Unlike 'Client.SendTransaction', no client verification key is needed
-- because the on-chain script does not verify any signature.
data SendTransaction = SendTransaction
  { ustCoreCfgFile    :: !FilePath
  , ustSigningKeyFile :: !FilePath
  , ustOutAddress     :: !GYAddress
  , ustOmniTransfer   :: !(Maybe (GYMintingPolicyId, Integer))
  , ustHashMode       :: !AsterizmHashMode
  , ustMessage        :: !BS.ByteString
  }

-- | Mint user token for outgoing message.
-- The minting policy is universal (not parameterized by any PKH)
-- and only validates that the token name matches the selected hash.
userSend :: SendTransaction -> IO ()
userSend (SendTransaction cfgFile skeyFile sendTo mOmniTransfer hashMode msg) = do
  coreCfg <- coreConfigIO cfgFile
  skey    <- readPaymentSigningKey skeyFile

  let nid = cfgNetworkId coreCfg

  let plutusPolicy       = asterizmUserCompiled
      (policy, policyId) = policyFromPlutus plutusPolicy

  let msgHash    = hashMessage hashMode msg
      tokenName  = fromJust $ tokenNameFromBS msgHash
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let inlineDatum = Just (datumFromPlutusData (toBuiltin msg), GYTxOutUseInlineDatum @PlutusV3)

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    w1 <- paymentUserWithCollateral nid providers skey
    let changeAddr = userChangeAddress w1

    skeleton <- case mOmniTransfer of
      Nothing -> pure $
          mustHaveOutput (GYTxOut sendTo tokenValue inlineDatum Nothing)
            <> mustMint policy (hashModeRedeemer hashMode) tokenName 1
      Just (omniPolicyId, amount) -> do
        if amount <= 0
        then throwIO $ userError "Omni-chain token amount must be positive."
        else pure ()
        let omniToken = GYToken omniPolicyId omniTokenNameGY
            omniValue = valueSingleton omniToken amount
        ownUtxos <- runGYTxQueryMonadIO nid providers $ utxosAtAddress changeAddr Nothing
        omniUtxo <- case filter (\u -> valueAssetClass (utxoValue u) omniToken >= amount) $ utxosToList ownUtxos of
          u : _ -> pure u
          _     -> throwIO $ userError "No own UTxO contains enough omni-chain tokens to attach."
        pure $
          mustHaveInput (GYTxIn @PlutusV3 (utxoRef omniUtxo) GYTxInWitnessKey)
            <> mustHaveOutput (GYTxOut sendTo (omniValue <> tokenValue) inlineDatum Nothing)
            <> mustMint policy (hashModeRedeemer hashMode) tokenName 1

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
