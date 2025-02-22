module ZkFold.Cardano.Balancing.Transaction.Transfer where

import           Data.Aeson                              (encode, encodeFile)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           GeniusYield.Transaction.Common          (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types                       (GYAddress, GYNetworkId, GYPaymentSigningKey,
                                                          GYProviders, GYScript, GYTxIn, GYTxOut (..),
                                                          GYTxOutUseInlineDatum (..), PlutusVersion (..),
                                                          datumFromPlutusData, gyGetProtocolParameters,
                                                          valueFromLovelace, addressFromValidator)
import           PlutusLedgerApi.V3                      (BuiltinByteString)
import           Prelude                                 (FilePath, IO, Integral (..), Maybe (..),
                                                          Semigroup (..), ($))
import           System.FilePath                         ((</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (dataToCBOR, savePlutus)
import           ZkFold.Cardano.UPLC.Common              (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx   (plonkupVerifierTxCompiled)
import           Data.Aeson                              (encode, encodeFile)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.String                             (fromString)
import           GeniusYield.Transaction.Common          (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types                       (GYAddress, GYAnyScript (..), GYNetworkId, GYPaymentSigningKey,
                                                          GYProviders, GYScript, GYTxIn (..), GYTxOut (..),
                                                          GYTxOutUseInlineDatum (..), PlutusVersion (..),
                                                          datumFromPlutusData, gyGetProtocolParameters,
                                                          valueFromLovelace)
import           PlutusLedgerApi.V3                      (BuiltinByteString, Data (..), Datum (..), dataToBuiltinData)
import           Prelude                                 (FilePath, IO, Integral (..), Maybe (..),
                                                          Semigroup (..), ($), (.), Either (..), undefined)
import           System.FilePath                         ((</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (dataToCBOR, savePlutus)
import           ZkFold.Cardano.UPLC.Common              (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx   (plonkupVerifierTxCompiled)
import Cardano.Api
import Cardano.CLI.Types.Common (WitnessSigningData)
import Cardano.CLI.Read (SomeSigningWitness(..), readWitnessSigningData)
import GeniusYield.GYConfig
import GeniusYield.Types.Script
import GeniusYield.Types.Address
import GeniusYield.Types.TxOutRef
import GeniusYield.Types.Key
import GeniusYield.Types.TxIn (GYTxInWitness(..))


data Transaction = Transaction
    { curPath         :: !FilePath
    , pathCoreCfg     :: !FilePath
    , txIn            :: !TxIn
    , requiredSigners :: !WitnessSigningData
    , changeAddresses :: !AddressAny
    , outAddress      :: !AddressAny
    , outFile         :: !FilePath
    }

-- | Sending a compiled script to the network.
initialTransfer ::
    GYNetworkId ->
    GYProviders ->
    GYPaymentSigningKey ->
    GYAddress ->
    GYTxIn PlutusV3 ->
    GYScript 'PlutusV3 ->
    BuiltinByteString ->
    FilePath ->
    IO ()
initialTransfer nid providers skey changeAddr txIn validator datum outFile = do
    let w1 = User' skey Nothing changeAddr
        validdatorAddr = addressFromValidator nid validator
        inlineDatum = Just (datumFromPlutusData datum, GYTxOutUseInlineDatum @PlutusV3)
        outMinDatum = GYTxOut validdatorAddr (valueFromLovelace 0) inlineDatum Nothing

    params <- gyGetProtocolParameters providers
    let calculateMinDatum = valueFromLovelace $ toInteger $ minimumUTxO params outMinDatum

    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = mustHaveInput txIn
                <> mustHaveOutput (GYTxOut validdatorAddr calculateMinDatum inlineDatum Nothing)
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    encodeFile outFile txid


{-
tokenInit :: Transaction -> IO ()
tokenInit (Transaction path pathCfg txIn sig changeAddr outAddress outFile) = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary

    let contract = EqualityCheckContract x ps targetValue
        testData = path </> "test-data"
    BL.writeFile (testData </> "plonkup-raw-contract-data.json") $ encode contract
    let (setup, _, _) = equalityCheckVerificationBytes x ps targetValue

    coreCfg <- coreConfigIO pathCfg
    (Right (APaymentSigningWitness sks)) <- readWitnessSigningData sig

    let fmLabel     = 0  -- Use a different label (number) to get another 'forwardingMint' address
        nid         = cfgNetworkId coreCfg
        skey        = signingKeyFromApi sks
        changeAddr' = addressFromApi changeAddr
        txIn'       = GYTxIn (txOutRefFromApi txIn) GYTxInWitnessKey
        sendTo      = addressFromApi outAddress

        plonkupToken   = validatorFromPlutus $ plonkupVerifierTokenCompiled setup
        forwardingMint = validatorFromPlutus $ forwardingMintCompiled fmLabel

    withCfgProviders coreCfg "main" $ \providers -> do
        sendScript nid providers skey changeAddr' txIn' sendTo plonkupToken outFile
        sendScript nid providers skey changeAddr' txIn' sendTo forwardingMint outFile

-}

balancingInit :: Transaction -> IO ()
balancingInit (Transaction path pathCfg txIn sig changeAddr outAddress outFile) = do
    undefined
{-}
    x           <- generate arbitrary
    ps          <- generate arbitrary

    let contract = IdentityCircuitContract x ps
        testData = path </> "test-data"
    BL.writeFile (testData </> "plonkupVerifierTx-contract-data.json") $ encode contract
    let (setup, _, _) = identityCircuitVerificationBytes x ps

    coreCfg <- coreConfigIO pathCfg
    (Right (APaymentSigningWitness sks)) <- readWitnessSigningData sig

    let fmLabel     = 0  -- Use a different label (number) to get another 'forwardingMint' address
        nid         = cfgNetworkId coreCfg
        skey        = signingKeyFromApi sks
        changeAddr' = addressFromApi changeAddr
        txIn'       = GYTxIn (txOutRefFromApi txIn) GYTxInWitnessKey
        sendTo      = addressFromApi outAddress

        -- plonkupTx   = validatorFromPlutus $ plonkupVerifierTxCompiled setup
        parkingSpot = validatorFromPlutus $ parkingSpotCompiled 54
        someDatum = Datum . dataToBuiltinData $ Constr 0 [B $ fromString "deadbeef"]

    withCfgProviders coreCfg "main" $ \providers -> do
        undefined -- initialTransfer parkScript nid providers skey changeAddr' txIn' sendTo parkingSpot someDatum outFile
-}

    -- savePlutus (assets </> "plonkupVerifierTx.plutus") $ plonkupVerifierTxCompiled setup
    -- savePlutus (assets </> "parkingSpot.plutus") $ parkingSpotCompiled 54

    -- BS.writeFile (assets </> "unit.cbor") $ dataToCBOR ()
    -- BS.writeFile (assets </> "someDatum.cbor") $ dataToCBOR someDatum