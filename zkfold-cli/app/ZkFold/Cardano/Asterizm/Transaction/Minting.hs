module ZkFold.Cardano.Asterizm.Transaction.Minting where

import           Control.Monad                 (when)
import           Data.Aeson                    (encodeFile)
import qualified Data.ByteString.Char8         as BS8
import           Data.Maybe                    (fromJust)
import           GeniusYield.GYConfig          (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3            as V3
import           PlutusTx.Prelude              (blake2b_256)
import           Prelude
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Options.Common
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmMessageCompiled)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CoreConfigAlt
  , requiredSigner :: !SigningKeyAlt
  , outAddress     :: !GYAddress
  , message        :: !String
  , submitTx       :: !Bool
  }

mint :: Transaction -> IO ()
mint (Transaction path coreCfg' sig sendTo msg doSubmit) = do
  let assetsPath = path </> "assets"

  coreCfg <- fromCoreConfigAltIO coreCfg'
  skey    <- fromSigningKeyAltIO sig

  let nid = cfgNetworkId coreCfg

  let plutusPolicy = asterizmMessageCompiled
      mintScript   = scriptFromPlutus @PlutusV3 plutusPolicy
      policy       = GYMintScript @PlutusV3 mintScript
      policyId     = mintingPolicyIdFromWitness policy

  let msgBS    = toBuiltin $ BS8.pack msg
      redeemer = redeemerFromPlutusData msgBS

  let pkh        = pubKeyHash $ paymentVerificationKey skey
      changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash pkh
      w1         = User' skey Nothing changeAddr

  let tokenName' = blake2b_256 $ (getPubKeyHash $ pubKeyHashToPlutus pkh) <> msgBS
      tokenName  = fromJust . tokenNameFromBS $ fromBuiltin tokenName'
      token      = GYToken policyId tokenName
      tokenValue = valueSingleton token 1

  let skeleton = mustHaveOutput (GYTxOut sendTo  tokenValue Nothing Nothing)
              <> mustMint policy redeemer tokenName 1
              <> mustBeSignedBy pkh

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    txbody <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1
                                 (buildTxBody skeleton)

    putStr $ "\nEstimated transaction fee: " ++ (show $ txBodyFee txbody) ++ " Lovelace\n"

    when doSubmit $ do
      txid <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1
                                 (signAndSubmitConfirmed txbody)

      putStr $ "Transaction Id: " ++ show txid ++ "\n\n"
      encodeFile (assetsPath </> "asterizm-mint.tx") txid
