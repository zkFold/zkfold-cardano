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

import           ZkFold.Cardano.Asterizm.Types (AsterizmMessage (..))
import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import qualified ZkFold.Cardano.CLI.Parsers    as CLI
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmRelayerCompiled)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CLI.CoreConfigAlt
  , requiredSigner :: !CLI.SigningKeyAlt
  , outAddress     :: !GYAddress
  , publicFile     :: !FilePath
  , outFile        :: !FilePath
  }

relayerMint :: Transaction -> IO ()
relayerMint (Transaction path coreCfg' sig sendTo pubFile outFile) = do
  let assetsPath = path </> "assets"

  coreCfg <- CLI.fromCoreConfigAltIO coreCfg'
  skey    <- CLI.fromSigningKeyAltIO sig

  let nid = cfgNetworkId coreCfg

  let pkh        = pubKeyHash $ paymentVerificationKey skey
      changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash pkh
      w1         = User' skey Nothing changeAddr

  mMsgHash <- decodeFileStrict (assetsPath </> pubFile)

  msgHash <- case mMsgHash of
    Just (AsterizmMessage mh) -> pure mh
    Nothing                   -> throwIO $ userError "Unable to retrieve public message hash."

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
    encodeFile (assetsPath </> outFile) txid
