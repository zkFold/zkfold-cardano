{-# LANGUAGE TemplateHaskell #-}

module ZkFold.Cardano.Asterizm.Transaction.InitTransfer where

import           Control.Exception             (throwIO)
import           Data.Aeson                    (decodeFileStrict, encodeFile)
import           Data.Coerce                   (coerce)
import           Data.Maybe                    (fromJust)
import           GeniusYield.GYConfig          (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V1.Value      (lovelaceValue)
import           PlutusLedgerApi.V3            as V3
import           Prelude
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (fromAsterizmAdminParams, fromAsterizmClientParams)
import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import qualified ZkFold.Cardano.CLI.Parsers    as CLI
import           ZkFold.Cardano.UPLC.Asterizm  (AsterizmAdmin (..), AsterizmTransferMeta (..), AsterizmTxId (..),
                                                InitThreadRedeemer (..), asterizmInitCompiled, asterizmInitThreadPolicy)

data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CLI.CoreConfigAlt
  , requiredSigner :: !CLI.SigningKeyAlt
  , outFile        :: !FilePath
  }

initTransfer :: Transaction -> IO ()
initTransfer (Transaction path coreCfg' sig outFile) = do
  let assetsPath = path </> "assets"
      setupFile  = assetsPath </> "asterizm-setup.json"
      adminFile  = assetsPath </> "asterizm-admin.json"

  mAsterizmAdminParams  <- decodeFileStrict adminFile
  mAsterizmClientParams <- decodeFileStrict setupFile

  asterizmAdmin <- case mAsterizmAdminParams of
    Just aap -> pure $ fromAsterizmAdminParams aap
    Nothing  -> throwIO $ userError "Unable to decode Asterizm admin file."

  asterizmSetup <- case mAsterizmClientParams of
    Just acp -> pure $ fromAsterizmClientParams acp
    Nothing  -> throwIO $ userError "Unable to decode Asterizm setup file."

  coreCfg <- CLI.fromCoreConfigAltIO coreCfg'
  skey    <- CLI.fromSigningKeyAltIO sig

  let nid = cfgNetworkId coreCfg

  let pkh        = pubKeyHash $ paymentVerificationKey skey
      changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash pkh
      w1         = User' skey Nothing changeAddr

  let plutusPolicy       = asterizmInitThreadPolicy asterizmAdmin asterizmSetup
      (policy, policyId) = policyFromPlutus plutusPolicy

  let plutusScript         = asterizmInitCompiled asterizmAdmin $
                             mintingPolicyIdToCurrencySymbol policyId
      transferContract     = scriptFromPlutus @PlutusV3 plutusScript
      transferContractAddr = addressFromScript nid transferContract

  let asterizmTransferData :: AsterizmTransferMeta
      asterizmTransferData = undefined  --ToDo

  let tokenName  = fromJust . tokenNameFromBS . fromBuiltin @BuiltinByteString . coerce $
                   atmTxId asterizmTransferData
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let asterizmFee = either (const mempty) id . valueFromPlutus . lovelaceValue $
                    aaAsterizmFee asterizmAdmin
      initTransferValue = asterizmFee <> tokenValue

  let inlineDatum = Just (datumFromPlutusData asterizmTransferData, GYTxOutUseInlineDatum @PlutusV3)

  let initRedeemer = redeemerFromPlutusData Init

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    let skeleton = mustHaveOutput (GYTxOut transferContractAddr initTransferValue inlineDatum Nothing)
                <> mustMint policy initRedeemer tokenName 1
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
