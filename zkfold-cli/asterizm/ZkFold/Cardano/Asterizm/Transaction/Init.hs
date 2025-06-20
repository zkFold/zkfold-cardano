module ZkFold.Cardano.Asterizm.Transaction.Init where

import           Data.Aeson                    (encode, encodeFile)
import qualified Data.ByteString.Lazy          as BL
import           Data.String                   (fromString)
import           GeniusYield.GYConfig          (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import qualified PlutusLedgerApi.V2            as V2
import           PlutusLedgerApi.V3            as V3
import           Prelude
import           System.Directory              (createDirectoryIfMissing, doesFileExist)
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (AsterizmParams (..))
import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import           ZkFold.Cardano.Options.Common
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmRelayerCompiled)
import           ZkFold.Cardano.UPLC.Common    (nftPolicyCompiled)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CoreConfigAlt
  , requiredSigner :: !SigningKeyAlt
  , nftOref        :: !GYTxOutRef
  , outAddress     :: !GYAddress
  , clientPKH      :: !GYPubKeyHash
  , relayerPKHs    :: ![GYPubKeyHash]
  , outFile        :: !FilePath
  }

asterizmInit :: Transaction -> IO ()
asterizmInit (Transaction path coreCfg' sig nftOref sendTo clientPKH relayerPKHs outFile) = do
  let assetsPath = path </> "assets"
      setupFile  = assetsPath </> "asterizm-setup.json"

  createDirectoryIfMissing True assetsPath
  setupFileExists <- doesFileExist setupFile

  if setupFileExists
    then putStr $ "\nSetup file " ++ setupFile ++ " already exists.\n\n"
    else do
      coreCfg <- fromCoreConfigAltIO coreCfg'
      skey    <- fromSigningKeyAltIO sig

      let nid = cfgNetworkId coreCfg

      let pkh        = pubKeyHash $ paymentVerificationKey skey
          changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash pkh
          w1         = User' skey Nothing changeAddr

      let nftPlutusPolicy          = nftPolicyCompiled . bumpTxOutRef $ txOutRefToPlutus nftOref
          (nftPolicy, nftPolicyId) = policyFromPlutus nftPlutusPolicy

      let nftCS = mintingPolicyIdToCurrencySymbol nftPolicyId

      let asterizmParams = AsterizmParams
            { apClientPKH    = fromBuiltin . getPubKeyHash $ pubKeyHashToPlutus clientPKH
            , apThreadSymbol = fromBuiltin $ unCurrencySymbol nftCS
            }

      BL.writeFile setupFile $ encode asterizmParams

      let nftName     = fromString "Asterizm Registry"
          threadValue = valueSingleton (GYToken nftPolicyId nftName) 1

      let relayerCSs = mintingPolicyIdToCurrencySymbol . snd . policyFromPlutus
                       . asterizmRelayerCompiled . pubKeyHashToPlutus <$> relayerPKHs

      let inlineDatum = Just (datumFromPlutusData relayerCSs, GYTxOutUseInlineDatum @PlutusV3)

      let skeleton = mustHaveInput (GYTxIn nftOref GYTxInWitnessKey)
                  <> mustHaveOutput (GYTxOut sendTo threadValue inlineDatum Nothing)
                  <> mustMint nftPolicy unitRedeemer nftName 1
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


------------------------------- :Helpers: --------------------------------

bumpTxId :: V2.TxId -> V3.TxId
bumpTxId (V2.TxId a) = V3.TxId a

bumpTxOutRef :: V2.TxOutRef -> V3.TxOutRef
bumpTxOutRef (V2.TxOutRef a b) = V3.TxOutRef (bumpTxId a) b
