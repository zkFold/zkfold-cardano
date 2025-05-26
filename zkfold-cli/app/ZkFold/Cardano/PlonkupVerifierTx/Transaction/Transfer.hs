module ZkFold.Cardano.PlonkupVerifierTx.Transaction.Transfer where

import           Control.Exception                       (throwIO)
import           Data.Aeson                              (decode, encode)
import qualified Data.ByteString.Lazy                    as BL
import           Data.Maybe                              (fromJust, isJust)
import           GeniusYield.GYConfig                    (GYCoreConfig (cfgNetworkId), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Prelude
import           System.Directory                        (createDirectoryIfMissing, doesFileExist)
import           System.FilePath                         ((</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Atlas.Utils              (SubmittedTx (..), wrapUpSubmittedTx)
import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes)
import           ZkFold.Cardano.OnChain.Plonkup.Data     (SetupBytes)
import           ZkFold.Cardano.Options.Common
import           ZkFold.Cardano.PlonkupVerifierTx.Types
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx   (plonkupVerifierTxCompiled)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CoreConfigAlt
  , reward         :: !GYValue
  , requiredSigner :: !SigningKeyAlt
  , changeAddress  :: !GYAddress
  , outFile        :: !FilePath
  }

verifierTransfer :: Transaction -> IO ()
verifierTransfer (Transaction path coreCfg' reward sig changeAddr outFile) = do
  let assetsPath = path </> "assets"
      setupFile  = assetsPath </> "plonkupVerifierTx-setup-data.json"

  coreCfg <- fromCoreConfigAltIO coreCfg'
  skey    <- fromSigningKeyAltIO sig

  let nid = cfgNetworkId coreCfg
      w1  = User' skey Nothing changeAddr

  PlonkupVerifierTxSetup x <- fromJust . decode <$> BL.readFile setupFile
  ps <- generate arbitrary

  let (setup, _, _) = identityCircuitVerificationBytes x ps
  let plutusValidator = plonkupVerifierTxCompiled setup
      script          = scriptFromPlutus @PlutusV3 plutusValidator
      scriptAddr      = addressFromValidator nid script

  pkh <- addressToPubKeyHashIO changeAddr
  let skeleton = mustHaveOutput (GYTxOut scriptAddr reward Nothing Nothing)
              <> mustBeSignedBy pkh

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    tx <- runGYTxGameMonadIO nid
                             providers $
                             asUser w1 $ do
                               txbody <- buildTxBody skeleton
                               txid   <- signAndSubmitConfirmed txbody
                               return $ SubmittedTx txid (Just $ txBodyFee txbody)

    wrapUpSubmittedTx outFile tx
