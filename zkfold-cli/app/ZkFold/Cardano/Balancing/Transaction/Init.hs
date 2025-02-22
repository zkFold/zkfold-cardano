module ZkFold.Cardano.Balancing.Transaction.Init (balancingInit, Transaction(..)) where

import           Cardano.Api                    (AddressAny, TxIn)
import           Cardano.CLI.Read               (SomeSigningWitness (..), readWitnessSigningData)
import           Cardano.CLI.Types.Common       (WitnessSigningData)
import           Data.Aeson                     (encodeFile)
import           Data.String                    (fromString)
import           GeniusYield.GYConfig           (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.Transaction.Common (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types              (GYNetworkId, GYProviders, GYTxIn (..), GYTxOut (..),
                                                 GYTxOutUseInlineDatum (..), PlutusVersion (..), datumFromPlutusData,
                                                 gyGetProtocolParameters, valueFromLovelace)
import           GeniusYield.Types.Address      (GYAddress, addressFromApi, addressFromValidator)
import           GeniusYield.Types.Key          (GYPaymentSigningKey, signingKeyFromApi)
import           GeniusYield.Types.Script       (GYAnyScript (..), GYScript, validatorFromPlutus)
import           GeniusYield.Types.TxIn         (GYTxInWitness (..))
import           GeniusYield.Types.TxOutRef     (txOutRefFromApi)
import           PlutusLedgerApi.V3             (Data (..), Datum (..), dataToBuiltinData)
import           Prelude                        (Either (..), FilePath, IO, Integral (..), Maybe (..), Semigroup (..),
                                                 ($), (.))

import           ZkFold.Cardano.UPLC.Common     (parkingSpotCompiled)

data Transaction = Transaction
    { pathCoreCfg     :: !FilePath
    , txIn            :: !TxIn
    , requiredSigners :: !WitnessSigningData
    , changeAddresses :: !AddressAny
    , outFile         :: !FilePath
    }

-- | Sending a compiled script to the network.
parkScript ::
    GYNetworkId ->
    GYProviders ->
    GYPaymentSigningKey ->
    GYAddress ->
    GYTxIn PlutusV3 ->
    GYAddress ->
    GYScript 'PlutusV3 ->
    Datum ->
    FilePath ->
    IO ()
parkScript nid providers skey changeAddr txIn sendTo validator datum outFile = do
    let w1 = User' skey Nothing changeAddr

        validator' = Just $ GYPlutusScript validator
        outMinScript = GYTxOut sendTo (valueFromLovelace 0) Nothing validator'

        inlineDatum = Just (datumFromPlutusData datum, GYTxOutUseInlineDatum @PlutusV3)
        outMinDatum = GYTxOut sendTo (valueFromLovelace 0) inlineDatum Nothing

    params <- gyGetProtocolParameters providers
    let calculateMinScript = valueFromLovelace $ toInteger $ minimumUTxO params outMinScript
        calculateMinDatum  = valueFromLovelace $ toInteger $ minimumUTxO params outMinDatum

    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = mustHaveInput txIn
                <> mustHaveOutput (GYTxOut sendTo calculateMinScript Nothing validator')
                <> mustHaveOutput (GYTxOut sendTo calculateMinDatum inlineDatum Nothing)
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    encodeFile outFile txid

balancingInit :: Transaction -> IO ()
balancingInit (Transaction pathCfg txIn sig changeAddr outFile) = do
    coreCfg <- coreConfigIO pathCfg
    (Right (APaymentSigningWitness sks)) <- readWitnessSigningData sig

    let nid         = cfgNetworkId coreCfg
        skey        = signingKeyFromApi sks
        changeAddr' = addressFromApi changeAddr
        txIn'       = GYTxIn (txOutRefFromApi txIn) GYTxInWitnessKey

        parkingSpot = validatorFromPlutus $ parkingSpotCompiled 54
        sendTo      = addressFromValidator nid parkingSpot
        someDatum   = Datum . dataToBuiltinData $ Constr 0 [B $ fromString "deadbeef"]

    withCfgProviders coreCfg "main" $ \providers -> do
        parkScript nid providers skey changeAddr' txIn' sendTo parkingSpot someDatum outFile
