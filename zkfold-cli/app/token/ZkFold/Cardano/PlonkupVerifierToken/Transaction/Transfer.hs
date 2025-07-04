module ZkFold.Cardano.PlonkupVerifierToken.Transaction.Transfer (tokenTransfer, Transaction(..)) where

import           Control.Exception                     (throwIO)
import           GeniusYield.GYConfig                  (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.Transaction.Common        (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Prelude
import           System.FilePath                       ((</>))

import           ZkFold.Cardano.CLI.Parsers            (CoreConfigAlt, SigningKeyAlt, fromCoreConfigAltIO,
                                                        fromSigningKeyAltIO)
import           ZkFold.Cardano.CLI.Utils              (SubmittedTx (..), wrapUpSubmittedTx)
import           ZkFold.Cardano.Options.Common         (PolicyIdAlt, fromPolicyIdAltIO)
import           ZkFold.Cardano.UPLC.ForwardingScripts (forwardingMintCompiled)


data Transaction = Transaction
    { curPath        :: !FilePath
    , coreCfg        :: !CoreConfigAlt
    , fmTag          :: !Integer
    , policyId       :: !PolicyIdAlt
    , reward         :: !GYValue
    , requiredSigner :: !SigningKeyAlt
    , changeAddress  :: !GYAddress
    , outFile        :: !FilePath
    }

-- | Sending a datum script to the network.
sendDatum :: GYNetworkId         ->
             GYProviders         ->
             GYPaymentSigningKey ->
             -- ^ Signing key for wallet funding this Tx.
             GYAddress           ->
             -- ^ Change address for wallet funding this Tx.
             GYValue             ->
             -- ^ Lovelace value of reward
             GYScript PlutusV3   ->
             -- ^ Parameterized 'ForwardingMint' script.
             GYMintingPolicyId   ->
             -- ^ PolicyID to be included in datum.
             FilePath            ->
             -- ^ Path to output file.
             IO ()
sendDatum nid providers skey changeAddr reward fmValidator policyid outFile = do
    let w1 = User' skey Nothing changeAddr

    let cs = mintingPolicyIdToCurrencySymbol policyid
        inlineDatum = Just (datumFromPlutusData cs, GYTxOutUseInlineDatum @PlutusV3)

    let forwardingMintAddr = addressFromValidator nid fmValidator

    params <- gyGetProtocolParameters providers
    let outMin          = GYTxOut forwardingMintAddr (valueFromLovelace 0) inlineDatum Nothing
        minUtxoLovelace = toInteger $ minimumUTxO params outMin

    if valueAda reward >= minUtxoLovelace
      then do
        pkh <- addressToPubKeyHashIO changeAddr
        let skeleton = mustHaveOutput (GYTxOut forwardingMintAddr reward inlineDatum Nothing)
                    <> mustBeSignedBy pkh

        tx <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1 $ do
                                   txbody <- buildTxBody skeleton
                                   txid   <- signAndSubmitConfirmed txbody
                                   return $ SubmittedTx txid (Just $ txBodyFee txbody)

        wrapUpSubmittedTx outFile tx

      else throwIO $ userError "Reward must be at least minimumUTxO lovelace."

tokenTransfer :: Transaction -> IO ()
tokenTransfer (Transaction path coreCfg' tag pid reward sig changeAddr outFile) = do
    let assetsPath = path </> "assets"

    coreCfg  <- fromCoreConfigAltIO coreCfg'
    policyid <- fromPolicyIdAltIO assetsPath pid
    skey     <- fromSigningKeyAltIO sig

    let nid            = cfgNetworkId coreCfg
        forwardingMint = scriptFromPlutus $ forwardingMintCompiled tag

    withCfgProviders coreCfg "zkfold-cli" $ \providers -> sendDatum
                                                            nid
                                                            providers
                                                            skey
                                                            changeAddr
                                                            reward
                                                            forwardingMint
                                                            policyid
                                                            (assetsPath </> outFile)
