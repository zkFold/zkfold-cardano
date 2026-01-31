{-# LANGUAGE TemplateHaskell #-}

module ZkFold.Cardano.Asterizm.Transaction.InitRelay where

import           Control.Exception             (throwIO)
import           Data.Aeson                    (decodeFileStrict, encodeFile)
import qualified Data.ByteString.Char8         as BS
import           GeniusYield.GYConfig          (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V1.Value      (lovelaceValue)
import           PlutusLedgerApi.V3            as V3
import           Prelude
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (fromAsterizmAdminParams)
import qualified ZkFold.Cardano.CLI.Parsers    as CLI
import           ZkFold.Cardano.UPLC.Asterizm  (AsterizmAdmin (..), AsterizmTransferMeta (..), AsterizmTxId (..),
                                                ChainAddress (..), ChainId (..), TransferHash (..),
                                                asterizmInitCompiled)

data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CLI.CoreConfigAlt
  , requiredSigner :: !CLI.SigningKeyAlt
  , srcChainId     :: !Integer
  , srcAddress     :: !BS.ByteString
  , dstChainId     :: !Integer
  , dstAddress     :: !BS.ByteString
  , asterizmTxId   :: !BS.ByteString
  , notifyFlag     :: !Bool
  , transferHash   :: !BS.ByteString
  , outFile        :: !FilePath
  }

initRelay :: Transaction -> IO ()
initRelay ( Transaction
            path coreCfg' sig
            srcChainId srcAddress dstChainId dstAddress asterizmTxId notifyFlag transferHash  -- Field values for 'AsterizmTransferMeta'
            outFile
          ) = do
  let assetsPath = path </> "assets"
      adminFile  = assetsPath </> "asterizm-admin.json"

  mAsterizmAdminParams  <- decodeFileStrict adminFile

  asterizmAdmin <- case mAsterizmAdminParams of
    Just aap -> pure $ fromAsterizmAdminParams aap
    Nothing  -> throwIO $ userError "Unable to decode Asterizm admin file."

  coreCfg <- CLI.fromCoreConfigAltIO coreCfg'
  skey    <- CLI.fromSigningKeyAltIO sig

  let nid = cfgNetworkId coreCfg

  let pkh        = pubKeyHash $ paymentVerificationKey skey
      changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash pkh
      w1         = User' skey Nothing changeAddr

  let plutusScript         = asterizmInitCompiled asterizmAdmin
      transferContract     = scriptFromPlutus @PlutusV3 plutusScript
      transferContractAddr = addressFromScript nid transferContract

  let asterizmTransferData :: AsterizmTransferMeta
      asterizmTransferData = AsterizmTransferMeta
        { atmSrcChainId   = ChainId srcChainId
        , atmSrcAddress   = ChainAddress $ toBuiltin srcAddress
        , atmDstChainId   = ChainId dstChainId
        , atmDstAddress   = ChainAddress $ toBuiltin dstAddress
        , atmTxId         = AsterizmTxId $ toBuiltin asterizmTxId
        , atmNotifyFlag   = notifyFlag
        , atmTransferHash = TransferHash $ toBuiltin transferHash
        }

  let asterizmFee = either (const mempty) id . valueFromPlutus . lovelaceValue $
                    aaAsterizmFee asterizmAdmin

  let inlineDatum = Just (datumFromPlutusData asterizmTransferData, GYTxOutUseInlineDatum @PlutusV3)

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    let skeleton = mustHaveOutput (GYTxOut transferContractAddr asterizmFee inlineDatum Nothing)
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
