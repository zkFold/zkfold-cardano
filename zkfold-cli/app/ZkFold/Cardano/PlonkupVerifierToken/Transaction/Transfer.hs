module ZkFold.Cardano.PlonkupVerifierToken.Transaction.Transfer (tokenTransfer, Transaction(..)) where

import           Cardano.Api                           (AddressAny, SerialiseAsRawBytes (..), TxIn)
import           Cardano.Api.Ledger                    (toCBOR)
import           Cardano.CLI.Read                      (SomeSigningWitness (..), readWitnessSigningData)
import           Cardano.CLI.Types.Common              (WitnessSigningData)
import           Codec.CBOR.Write                      (toLazyByteString)
import qualified Codec.Serialise                       as Codec
import           Data.Aeson                            (encodeFile)
import           GeniusYield.GYConfig                  (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.Transaction.Common        (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types                     (GYAddress, GYNetworkId, GYPaymentSigningKey, GYProviders,
                                                        GYScript, GYTxIn (..), GYTxInWitness (..), GYTxOut (..),
                                                        GYTxOutUseInlineDatum (..), PlutusVersion (..), addressFromApi,
                                                        addressFromValidator, datumFromPlutusData,
                                                        gyGetProtocolParameters, mintingPolicyId, mintingPolicyIdToApi,
                                                        signingKeyFromApi, txOutRefFromApi, validatorFromPlutus,
                                                        valueFromLovelace)
import           PlutusTx.Builtins                     (BuiltinByteString)
import           Prelude                               (Either (..), FilePath, IO, Maybe (..), toInteger,
                                                        ($), (<>))

import           ZkFold.Cardano.UPLC.ForwardingScripts (forwardingMintCompiled)

data Transaction = Transaction
    { pathCoreCfg     :: !FilePath
    , txIn            :: !TxIn
    , requiredSigners :: !WitnessSigningData
    , changeAddresses :: !AddressAny
    , outFile         :: !FilePath
    }


-- | Sending a datum script to the network.
sendDatum ::
    GYNetworkId ->
    GYProviders ->
    GYPaymentSigningKey ->
    GYAddress ->
    GYTxIn PlutusV3 ->
    GYScript PlutusV3 ->
    BuiltinByteString ->
    FilePath ->
    IO ()
sendDatum nid providers skey changeAddr txIn validator datum outFile = do
    let w1 = User' skey Nothing changeAddr
        inlineDatum = Just (datumFromPlutusData datum, GYTxOutUseInlineDatum @PlutusV3)
        validdatorAddr = addressFromValidator nid validator
        outMin = GYTxOut validdatorAddr (valueFromLovelace 0) inlineDatum Nothing

    params <- gyGetProtocolParameters providers
    let calculateMin = valueFromLovelace $ toInteger $ minimumUTxO params outMin

    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = mustHaveInput txIn
                <> mustHaveOutput (GYTxOut validdatorAddr calculateMin inlineDatum Nothing)
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    encodeFile outFile txid

tokenTransfer :: Transaction -> IO ()
tokenTransfer (Transaction pathCfg txIn sig changeAddr outFile) = do
    coreCfg <- coreConfigIO pathCfg
    (Right (APaymentSigningWitness sks)) <- readWitnessSigningData sig

    let nid            = cfgNetworkId coreCfg
        skey           = signingKeyFromApi sks
        changeAddr'    = addressFromApi changeAddr
        txIn'          = GYTxIn (txOutRefFromApi txIn) GYTxInWitnessKey
        fmLabel        = 0

        forwardingMint = validatorFromPlutus $ forwardingMintCompiled fmLabel
        policyid       = mintingPolicyIdToApi $ mintingPolicyId forwardingMint
        datum          = Codec.deserialise $ toLazyByteString $ toCBOR $ serialiseToRawBytes policyid

    withCfgProviders coreCfg "main" $ \providers -> do
        sendDatum nid providers skey changeAddr' txIn' forwardingMint datum outFile
