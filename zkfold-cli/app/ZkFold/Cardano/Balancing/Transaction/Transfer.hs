module ZkFold.Cardano.Balancing.Transaction.Transfer (balancingTransfer, Transaction(..)) where

import           Cardano.Api
import           Cardano.CLI.Read                        (SomeSigningWitness (..), readWitnessSigningData)
import           Cardano.CLI.Type.Common                 (WitnessSigningData)
import           Data.Aeson                              (encode, encodeFile)
import qualified Data.ByteString.Lazy                    as BL
import           GeniusYield.GYConfig                    (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.Transaction.Common          (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types                       (GYNetworkId, GYProviders, GYTxIn (..), GYTxOut (..),
                                                          GYTxOutUseInlineDatum (..), PlutusVersion (..),
                                                          datumFromPlutusData, gyGetProtocolParameters,
                                                          valueFromLovelace)
import           GeniusYield.Types.Address               (GYAddress, addressFromApi, addressFromValidator)
import           GeniusYield.Types.Key                   (GYPaymentSigningKey, signingKeyFromApi)
import           GeniusYield.Types.Script                (validatorFromPlutus)
import           GeniusYield.Types.TxIn                  (GYTxInWitness (..))
import           GeniusYield.Types.TxOutRef              (txOutRefFromApi)
import           PlutusLedgerApi.V3                      (Datum (..), toData)
import           PlutusTx.Builtins                       (toBuiltin)
import           Prelude                                 (Either (..), FilePath, IO, Integral (..), Maybe (..),
                                                          Semigroup (..), ($))
import           System.FilePath                         ((</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes)
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx   (plonkupVerifierTxCompiled)

data Transaction = Transaction
    { curPath         :: !FilePath
    , pathCoreCfg     :: !FilePath
    , txIn            :: !TxIn
    , requiredSigners :: !WitnessSigningData
    , changeAddresses :: !AddressAny
    , outFile         :: !FilePath
    }

-- | Sending a compiled script to the network.
initialTransfer ::
    GYNetworkId ->
    GYProviders ->
    GYPaymentSigningKey ->
    GYAddress ->
    GYTxIn PlutusV3 ->
    GYAddress ->
    Datum ->
    FilePath ->
    IO ()
initialTransfer nid providers skey changeAddr txIn sendTo datum outFile = do
    let w1 = User' skey Nothing changeAddr
        inlineDatum = Just (datumFromPlutusData datum, GYTxOutUseInlineDatum @PlutusV3)
        outMinDatum = GYTxOut sendTo (valueFromLovelace 0) inlineDatum Nothing

    params <- gyGetProtocolParameters providers
    let calculateMinDatum = valueFromLovelace $ toInteger $ minimumUTxO params outMinDatum

    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = mustHaveInput txIn
                <> mustHaveOutput (GYTxOut sendTo calculateMinDatum inlineDatum Nothing)
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    encodeFile outFile txid

balancingTransfer :: Transaction -> IO ()
balancingTransfer (Transaction path pathCfg txIn sig changeAddr outFile) = do
    x           <- generate arbitrary
    ps          <- generate arbitrary

    let contract = IdentityCircuitContract x ps
        testData = path </> "test-data"
    BL.writeFile (testData </> "plonkupVerifierTx-contract-data.json") $ encode contract
    let (setup, _, _) = identityCircuitVerificationBytes x ps

    coreCfg <- coreConfigIO pathCfg
    (Right (APaymentSigningWitness sks)) <- readWitnessSigningData sig

    let nid         = cfgNetworkId coreCfg
        skey        = signingKeyFromApi sks
        changeAddr' = addressFromApi changeAddr
        txIn'       = GYTxIn (txOutRefFromApi txIn) GYTxInWitnessKey

        plonkupTxAddr = addressFromValidator nid $ validatorFromPlutus @PlutusV3 $ plonkupVerifierTxCompiled setup
        someDatum     = Datum $ toBuiltin $ toData ()

    withCfgProviders coreCfg "main" $ \providers -> initialTransfer nid providers skey changeAddr' txIn' plonkupTxAddr someDatum outFile
