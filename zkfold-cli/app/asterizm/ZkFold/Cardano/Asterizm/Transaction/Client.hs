module ZkFold.Cardano.Asterizm.Transaction.Client where

import           Control.Exception             (throwIO)
import           Control.Monad                 (forM)
import           Data.Aeson                    (decodeFileStrict, encodeFile)
import           Data.Maybe                    (fromJust)
import           GeniusYield.GYConfig          (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3            as V3
import           Prelude
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (AsterizmSetup (..), toByteString)
import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import qualified ZkFold.Cardano.CLI.Parsers    as CLI
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmClientIncomingCompiled, buildCrosschainHash)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CLI.CoreConfigAlt
  , requiredSigner :: !CLI.SigningKeyAlt
  , outAddress     :: !GYAddress
  , privateFile    :: !FilePath
  , outFile        :: !FilePath
  }

clientMint :: Transaction -> IO ()
clientMint (Transaction path coreCfg' sig sendTo privFile outFile) = do
  let assetsPath = path </> "assets"
      setupFile  = assetsPath </> "asterizm-setup.json"

  mAsterizmSetup <- decodeFileStrict setupFile

  asterizmSetup <- case mAsterizmSetup of
    Just as -> pure as
    Nothing -> throwIO $ userError "Unable to decode Asterizm setup file."

  let relayerCSs = mintingPolicyIdToCurrencySymbol <$> acsAllowedRelayers asterizmSetup

  mMsg <- decodeFileStrict (assetsPath </> privFile)

  msg <- case mMsg of
    Just m  -> pure $ toBuiltin . toByteString $ m
    Nothing -> throwIO $ userError "Unable to retrieve private Asterizm message."

  coreCfg <- CLI.fromCoreConfigAltIO coreCfg'
  skey    <- CLI.fromSigningKeyAltIO sig

  let nid = cfgNetworkId coreCfg

  let pkh        = pubKeyHash $ paymentVerificationKey skey
      changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash pkh
      w1         = User' skey Nothing changeAddr

  let clientPKH        = pubKeyHashToPlutus $ acsClientPKH asterizmSetup
      allowedRelayers  = mintingPolicyIdToCurrencySymbol <$> acsAllowedRelayers asterizmSetup
      plutusPolicy     = asterizmClientIncomingCompiled clientPKH allowedRelayers
      (policy, policyId) = policyFromPlutus plutusPolicy

  let msgHash    = fromBuiltin . buildCrosschainHash $ msg
      tokenName  = fromJust $ tokenNameFromBS msgHash
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let inlineDatum = Just (datumFromPlutusData msg, GYTxOutUseInlineDatum @PlutusV3)

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
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
                <> mustBeSignedBy pkh

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
