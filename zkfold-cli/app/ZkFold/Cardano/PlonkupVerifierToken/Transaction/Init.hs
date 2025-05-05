module ZkFold.Cardano.PlonkupVerifierToken.Transaction.Init (Transaction(..), tokenInit) where

import           Data.Aeson                               (encode)
import qualified Data.ByteString.Lazy                     as BL
import           GeniusYield.GYConfig                     (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Prelude
import           System.FilePath                          ((</>))
import qualified System.IO                                as IO
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)

import           ZkFold.Cardano.Examples.EqualityCheck    (EqualityCheckContract (..), equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils            (currencySymbolOf)
import           ZkFold.Cardano.Options.Common            (CoreConfigAlt, SigningKeyAlt, SubmittedTx (..),
                                                           fromCoreConfigAltIO, fromSigningKeyAltIO, wrapUpSubmittedTx)
import           ZkFold.Cardano.UPLC.ForwardingScripts    (forwardingMintCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)


data Transaction = Transaction
    { curPath        :: !FilePath
    , coreCfgAlt     :: !CoreConfigAlt
    , fmTag          :: !Integer
    , requiredSigner :: !SigningKeyAlt
    , changeAddress  :: !GYAddress
    , outAddress     :: !GYAddress
    , outFile        :: !FilePath
    }

-- | Sending compiled scripts to the network.
sendScripts ::
    GYNetworkId ->
    GYProviders ->
    GYPaymentSigningKey ->
    -- ^ Signing key for wallet funding this Tx.
    GYAddress ->
    -- ^ Change address for wallet funding this Tx.
    GYAddress ->
    -- ^ Wallet address where validators will be parked at.
    [GYScript PlutusV3] ->
    -- ^ Validators.
    FilePath ->
    -- ^ Relative path to output file.
    IO ()
sendScripts nid providers skey changeAddr sendTo validators outFile = do
    let w1          = User' skey Nothing changeAddr
        validators' = Just . GYPlutusScript <$> validators

    pkh <- addressToPubKeyHashIO changeAddr

    let valTxOuts = GYTxOut sendTo mempty Nothing <$> validators'
        skeleton  = mconcat (mustHaveOutput <$> valTxOuts)
                 <> mustBeSignedBy pkh

    tx <- runGYTxGameMonadIO nid
                             providers $
                             asUser w1 $ do
                               txbody <- buildTxBody skeleton
                               txid   <- signAndSubmitConfirmed txbody
                               return $ SubmittedTx txid (Just $ txBodyFee txbody)

    wrapUpSubmittedTx outFile tx

tokenInit :: Transaction -> IO ()
tokenInit (Transaction path coreCfg' tag sig changeAddr sendTo outFile) = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary

    let contract = EqualityCheckContract x ps targetValue
        testData = path </> "test-data"
    BL.writeFile (testData </> "plonkup-raw-contract-data.json") $ encode contract
    let (setup, _, _) = equalityCheckVerificationBytes x ps targetValue

    coreCfg <- fromCoreConfigAltIO coreCfg'
    skey    <- fromSigningKeyAltIO sig

    let nid        = cfgNetworkId coreCfg
        assetsPath = path </> "assets"

    let plutusValidators = [plonkupVerifierTokenCompiled setup, forwardingMintCompiled tag]
        policyId         = show $ currencySymbolOf $ head plutusValidators
        validators       = scriptFromPlutus @PlutusV3 <$> plutusValidators

    IO.writeFile (assetsPath </> "plonkupVerifierTokenPolicyId.txt") policyId

    withCfgProviders coreCfg "zkfold-cli" $ \providers -> sendScripts
                                                            nid
                                                            providers
                                                            skey
                                                            changeAddr
                                                            sendTo
                                                            validators
                                                            (assetsPath </> outFile)
