module ZkFold.Cardano.Asterizm.Transaction.Init where

import           Data.Aeson                    (encode, encodeFile)
import qualified Data.ByteString.Lazy          as BL
import           Data.String                   (fromString)
import           GeniusYield.GYConfig          (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3            as V3
import           Prelude
import           System.Directory              (createDirectoryIfMissing, doesFileExist)
import           System.FilePath               ((</>))

import           ZkFold.Cardano.Asterizm.Types (AsterizmClientParams (..))
import           ZkFold.Cardano.Asterizm.Utils (policyFromPlutus)
import qualified ZkFold.Cardano.CLI.Parsers    as CLI
import           ZkFold.Cardano.CLI.Utils      (bumpTxOutRef)
import           ZkFold.Cardano.Options.Common
import           ZkFold.Cardano.UPLC.Asterizm  (asterizmRelayerCompiled)
import           ZkFold.Cardano.UPLC.Common    (nftPolicyCompiled)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CLI.CoreConfigAlt
  , requiredSigner :: !CLI.SigningKeyAlt
  , nftOref        :: !GYTxOutRef
  , outAddress     :: !GYAddress
  , clientPKH      :: !PubKeyHashAlt
  , relayerPKHs    :: ![PubKeyHashAlt]
  , outFile        :: !FilePath
  }

asterizmInit :: Transaction -> IO ()
asterizmInit (Transaction path coreCfg' sig nftOref sendTo clientPKHA relayerPKHAs outFile) = do
  let assetsPath = path </> "assets"
      setupFile  = assetsPath </> "asterizm-setup.json"

  createDirectoryIfMissing True assetsPath
  setupFileExists <- doesFileExist setupFile

  if setupFileExists
    then putStr $ "\nSetup file " ++ setupFile ++ " already exists.\n\n"
    else do
      coreCfg <- CLI.fromCoreConfigAltIO coreCfg'
      skey    <- CLI.fromSigningKeyAltIO sig

      let nid = cfgNetworkId coreCfg

      let pkh        = pubKeyHash $ paymentVerificationKey skey
          changeAddr = addressFromPaymentKeyHash nid $ fromPubKeyHash pkh
          w1         = User' skey Nothing changeAddr

      let nftPlutusPolicy          = nftPolicyCompiled . bumpTxOutRef $ txOutRefToPlutus nftOref
          (nftPolicy, nftPolicyId) = policyFromPlutus nftPlutusPolicy

      let nftCS = mintingPolicyIdToCurrencySymbol nftPolicyId

      clientPKH <- fromPubKeyHashAltIO clientPKHA

      let asterizmClientParams = AsterizmClientParams
            { acpClientPKH    = fromBuiltin . getPubKeyHash $ pubKeyHashToPlutus clientPKH
            , acpThreadSymbol = fromBuiltin $ unCurrencySymbol nftCS
            }

      BL.writeFile setupFile $ encode asterizmClientParams

      let nftName     = fromString "Asterizm Registry"
          threadValue = valueSingleton (GYToken nftPolicyId nftName) 1

      relayerPKHs <- mapM fromPubKeyHashAltIO relayerPKHAs

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
