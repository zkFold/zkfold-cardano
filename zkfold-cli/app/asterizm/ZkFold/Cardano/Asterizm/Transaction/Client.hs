module ZkFold.Cardano.Asterizm.Transaction.Client where

import           Control.Exception             (throwIO)
import           Control.Monad                 (forM)
import           Data.Aeson                    (decodeFileStrict, encodeFile)
import           Data.Maybe                    (fromJust)
import           Data.String                   (fromString)
import           GeniusYield.GYConfig          (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusCore.Crypto.Hash        (blake2b_256)
import           PlutusLedgerApi.V3            as V3
import           Prelude
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (HexByteString (..), fromAsterizmParams)
import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import qualified ZkFold.Cardano.CLI.Parsers    as CLI
import           ZkFold.Cardano.UPLC.Asterizm  (AsterizmSetup (..), asterizmClientCompiled)


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

  mAsterizmParams <- decodeFileStrict setupFile

  asterizmSetup <- case mAsterizmParams of
    Just ap -> pure $ fromAsterizmParams ap
    Nothing -> throwIO $ userError "Unable to decode Asterizm setup file."

  threadPolicyId <- case mintingPolicyIdFromCurrencySymbol $ acsThreadSymbol asterizmSetup of
    Right pid -> pure pid
    Left err  -> throwIO . userError $ "Thread-symbol error: " ++ (show err)

  mMsg <- decodeFileStrict (assetsPath </> privFile)

  msg <- case mMsg of
    Just (HexByteString m) -> pure m
    Nothing                -> throwIO $ userError "Unable to retrieve private Asterizm message."

  coreCfg <- CLI.fromCoreConfigAltIO coreCfg'
  skey    <- CLI.fromSigningKeyAltIO sig

  let nid = cfgNetworkId coreCfg

  let pkh        = pubKeyHash $ paymentVerificationKey skey
      changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash pkh
      w1         = User' skey Nothing changeAddr

  let plutusPolicy       = asterizmClientCompiled asterizmSetup
      (policy, policyId) = policyFromPlutus plutusPolicy

  let msgHash    = blake2b_256 msg
      tokenName  = fromJust $ tokenNameFromBS msgHash
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let inlineDatum = Just (datumFromPlutusData $ toBuiltin msg, GYTxOutUseInlineDatum @PlutusV3)

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    threadUtxos <- runGYTxQueryMonadIO nid providers $
      utxosWithAsset . GYNonAdaToken threadPolicyId $ fromString "Asterizm Registry"

    -- | Assumption: whenever the thread-token is consumed, datum with Relayer's Registry
    -- is updated correctly and pushed to the new UTxO where the thread-token is sent to.
    (threadOref, threadDatum) <- case utxosToList threadUtxos of
      [u] -> pure $ (utxoRef u, utxoOutDatum u)
      _   -> throwIO $ userError "Thread-symbol error: unable to locate thread-token."

    relayerCSs <- case threadDatum of
      GYOutDatumInline dat -> pure $ unsafeFromBuiltinData $ datumToPlutus' dat
      _                    -> throwIO $ userError "Unrecoverable relayers' registry."

    relayerTokens <- case mapM mintingPolicyIdFromCurrencySymbol relayerCSs of
      Right pids -> pure $ (\pid -> GYNonAdaToken pid tokenName) <$> pids
      Left _     -> throwIO $ userError "Corrupted relayers' registry."

    relayerUtxos <- forM relayerTokens $ runGYTxQueryMonadIO nid providers . utxosWithAsset

    relayerOref <- case concat $ map utxosToList relayerUtxos of
      u : _ -> pure $ utxoRef u
      _     -> throwIO $ userError "No relayer has validated client's message yet."

    let skeleton = mustHaveRefInput threadOref
                <> mustHaveRefInput relayerOref
                <> mustHaveOutput (GYTxOut sendTo tokenValue inlineDatum Nothing)
                <> mustMint policy unitRedeemer tokenName 1
                <> mustBeSignedBy pkh

    txbody <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1
                                 (buildTxBody skeleton)

    putStr $ "\nEstimated transaction fee: " ++ (show $ txBodyFee txbody) ++ " Lovelace\n"

    txid <- runGYTxGameMonadIO nid
                               providers $
                               asUser w1
                               (signAndSubmitConfirmed txbody)

    putStr $ "Transaction Id: " ++ show txid ++ "\n\n"
    encodeFile (assetsPath </> outFile) txid
