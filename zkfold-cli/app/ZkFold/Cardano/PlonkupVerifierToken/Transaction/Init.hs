module ZkFold.Cardano.PlonkupVerifierToken.Transaction.Init (Transaction(..), tokenInit) where

import           Cardano.Api                              (AddressAny, TxIn)
import           Cardano.CLI.Read                         (SomeSigningWitness (..), readWitnessSigningData)
import           Cardano.CLI.Types.Common                 (WitnessSigningData)
import           Data.Aeson                               (encode, encodeFile)
import qualified Data.ByteString.Lazy                     as BL
import           Data.Either                              (Either (..))
import           GeniusYield.GYConfig                     (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.Transaction.Common           (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types                        (GYAddress, GYAnyScript (..), GYNetworkId,
                                                           GYPaymentSigningKey, GYProviders, GYScript, GYTxIn (..),
                                                           GYTxInWitness (..), GYTxOut (..), PlutusVersion (..),
                                                           gyGetProtocolParameters, valueFromLovelace)
import           GeniusYield.Types.Address                (addressFromApi)
import           GeniusYield.Types.Key                    (signingKeyFromApi)
import           GeniusYield.Types.Script                 (validatorFromPlutus)
import           GeniusYield.Types.TxOutRef               (txOutRefFromApi)
import           Prelude                                  (FilePath, IO, Maybe (..),
                                                           toInteger, ($), (<>))
import           System.FilePath                          ((</>))
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)

import           ZkFold.Cardano.Examples.EqualityCheck    (EqualityCheckContract (..), equalityCheckVerificationBytes)
import           ZkFold.Cardano.UPLC.ForwardingScripts    (forwardingMintCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)

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
sendScript ::
    GYNetworkId ->
    GYProviders ->
    GYPaymentSigningKey ->
    GYAddress ->
    GYTxIn PlutusV3 ->
    GYAddress ->
    GYScript 'PlutusV3 ->
    FilePath ->
    IO ()
sendScript nid providers skey changeAddr txIn sendTo validator outFile = do
    let w1 = User' skey Nothing changeAddr
        validator' = Just $ GYPlutusScript validator
        outMin = GYTxOut sendTo (valueFromLovelace 0) Nothing validator'

    params <- gyGetProtocolParameters providers
    let calculateMin = valueFromLovelace $ toInteger $ minimumUTxO params outMin

    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = mustHaveInput txIn
                <> mustHaveOutput (GYTxOut sendTo calculateMin Nothing validator')
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    encodeFile outFile txid

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
