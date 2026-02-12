module ZkFold.Cardano.Asterizm.Transaction.Client where

import           Control.Exception             (throwIO)
import           Control.Monad                 (forM)
import           Data.Aeson                    (decodeFileStrict, encodeFile)
import qualified Data.ByteString               as BS
import           Data.Maybe                    (fromJust)
import           GeniusYield.GYConfig          (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3            as V3
import           Prelude
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (AsterizmSetup (..), MessageDirection (..), directionToBool)
import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import qualified ZkFold.Cardano.CLI.Parsers    as CLI
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmClientCompiled, buildCrosschainHash)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CLI.CoreConfigAlt
  , requiredSigner :: !CLI.SigningKeyAlt
  , outAddress     :: !GYAddress
  , message        :: !BS.ByteString
  , direction      :: !MessageDirection
  , outFile        :: !FilePath
  }

clientMint :: Transaction -> IO ()
clientMint (Transaction path coreCfg' sig sendTo msg dir outFile) = do
  let assetsPath = path </> "assets"
      setupFile  = assetsPath </> "asterizm-setup.json"

  mAsterizmSetup <- decodeFileStrict setupFile

  asterizmSetup <- case mAsterizmSetup of
    Just as -> pure as
    Nothing -> throwIO $ userError "Unable to decode Asterizm setup file."

  let relayerCSs = mintingPolicyIdToCurrencySymbol <$> acsAllowedRelayers asterizmSetup

  coreCfg <- CLI.fromCoreConfigAltIO coreCfg'
  skey    <- CLI.fromSigningKeyAltIO sig

  let nid = cfgNetworkId coreCfg

  let pkh        = pubKeyHash $ paymentVerificationKey skey
      changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash pkh
      w1         = User' skey Nothing changeAddr

  let clientPKH        = pubKeyHashToPlutus $ acsClientPKH asterizmSetup
      allowedRelayers  = mintingPolicyIdToCurrencySymbol <$> acsAllowedRelayers asterizmSetup
      isIncoming       = directionToBool dir
      plutusPolicy     = asterizmClientCompiled clientPKH allowedRelayers isIncoming
      (policy, policyId) = policyFromPlutus plutusPolicy

  let msgHash    = fromBuiltin . buildCrosschainHash . toBuiltin $ msg
      tokenName  = fromJust $ tokenNameFromBS msgHash
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let inlineDatum = Just (datumFromPlutusData (toBuiltin msg), GYTxOutUseInlineDatum @PlutusV3)

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    -- For incoming messages, find and reference the relayer's token
    mRelayerOref <- case dir of
      Incoming -> do
        relayerTokens <- case mapM mintingPolicyIdFromCurrencySymbol relayerCSs of
          Right pids -> pure $ (`GYNonAdaToken` tokenName) <$> pids
          Left _     -> throwIO $ userError "Corrupted relayers' registry."

        relayerUtxos <- forM relayerTokens $ runGYTxQueryMonadIO nid providers . utxosWithAsset

        case concatMap utxosToList relayerUtxos of
          u : _ -> pure $ Just (utxoRef u)
          _     -> throwIO $ userError "No relayer has validated client's message yet."

      Outgoing -> pure Nothing

    let baseSkeleton = mustHaveOutput (GYTxOut sendTo tokenValue inlineDatum Nothing)
                    <> mustMint policy unitRedeemer tokenName 1
                    <> mustBeSignedBy pkh

        skeleton = case mRelayerOref of
          Just oref -> mustHaveRefInput oref <> baseSkeleton
          Nothing   -> baseSkeleton

    txbody <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1
                                 (buildTxBody skeleton)

    putStr $ "\nEstimated transaction fee: " ++ show (txBodyFee txbody) ++ " Lovelace\n"

    txid <- runGYTxGameMonadIO nid
                               providers $
                               asUser w1
                               (signAndSubmitConfirmed txbody)

    putStr $ "Transaction Id: " ++ show txid ++ "\n\n"
    encodeFile (assetsPath </> outFile) txid
