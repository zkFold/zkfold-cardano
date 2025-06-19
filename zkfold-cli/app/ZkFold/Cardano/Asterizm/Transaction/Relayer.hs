module ZkFold.Cardano.Asterizm.Transaction.Relayer where

import           Control.Exception             (throwIO)
import           Data.Aeson                    (decodeFileStrict, encodeFile)
import           Data.Maybe                    (fromJust)
import           GeniusYield.GYConfig          (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3            as V3
import           Prelude
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (HexByteString (..))
import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import           ZkFold.Cardano.Options.Common
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmRelayerCompiled)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CoreConfigAlt
  , requiredSigner :: !SigningKeyAlt
  , outAddress     :: !GYAddress
  , publicFile     :: !FilePath
  }

relayerMint :: Transaction -> IO ()
relayerMint (Transaction path coreCfg' sig sendTo pubFile) = do
  let assetsPath = path </> "assets"

  coreCfg <- fromCoreConfigAltIO coreCfg'
  skey    <- fromSigningKeyAltIO sig

  let nid = cfgNetworkId coreCfg

  let pkh        = pubKeyHash $ paymentVerificationKey skey
      changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash pkh
      w1         = User' skey Nothing changeAddr

  mMsgHash <- decodeFileStrict (assetsPath </> pubFile)

  msgHash <- case mMsgHash of
    Just (HexByteString mh) -> pure mh
    Nothing                 -> throwIO $ userError "Unable to retrieve public message hash."

  let redeemer = redeemerFromPlutusData $ toBuiltin msgHash

  let plutusPolicy       = asterizmRelayerCompiled $ pubKeyHashToPlutus pkh
      (policy, policyId) = policyFromPlutus plutusPolicy

  let tokenName  = fromJust $ tokenNameFromBS msgHash
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let skeleton = mustHaveOutput (GYTxOut sendTo tokenValue Nothing Nothing)
              <> mustMint policy redeemer tokenName 1
              <> mustBeSignedBy pkh

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
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
    encodeFile (assetsPath </> "asterizm-relayer-mint.tx") txid
