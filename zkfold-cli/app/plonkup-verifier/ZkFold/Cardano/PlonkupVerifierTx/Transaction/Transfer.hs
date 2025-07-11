module ZkFold.Cardano.PlonkupVerifierTx.Transaction.Transfer where

import           Data.Aeson                              (decode)
import qualified Data.ByteString.Lazy                    as BL
import           Data.Maybe                              (fromJust)
import           GeniusYield.GYConfig                    (GYCoreConfig (cfgNetworkId), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Prelude
import           System.FilePath                         ((</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import qualified ZkFold.Cardano.CLI.Parsers              as CLI
import           ZkFold.Cardano.CLI.Utils                (SubmittedTx (..), wrapUpSubmittedTx)
import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes)
import           ZkFold.Cardano.PlonkupVerifierTx.Types
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx   (plonkupVerifierTxCompiled)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CLI.CoreConfigAlt
  , reward         :: !GYValue
  , requiredSigner :: !CLI.SigningKeyAlt
  , changeAddress  :: !GYAddress
  , outFile        :: !FilePath
  }

verifierTransfer :: Transaction -> IO ()
verifierTransfer (Transaction path coreCfg' reward sig changeAddr outFile) = do
  let assets    = path </> "assets"
      setupFile = assets </> "plonkupVerifierTx-setup-data.json"

  coreCfg <- CLI.fromCoreConfigAltIO coreCfg'
  skey    <- CLI.fromSigningKeyAltIO sig

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

    wrapUpSubmittedTx (assets </> outFile) tx
