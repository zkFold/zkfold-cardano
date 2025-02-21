module ZkFold.Cardano.PlonkupVerifierToken.Transaction.Init where

import           Cardano.Api                              (AddressAny, TxIn)
import           Cardano.CLI.Read                         (SomeSigningWitness (..), readWitnessSigningData)
import           Cardano.CLI.Types.Common                 (WitnessSigningData)
import           Data.Aeson                               (encode)
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
import           Prelude                                  (Bool (..), FilePath, IO, Maybe (..), Show (..), String,
                                                           error, print, toInteger, ($), (++), (<$>), (<>))
import           System.Directory                         (createDirectoryIfMissing)
import           System.FilePath                          ((</>))
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)

import           ZkFold.Cardano.Examples.EqualityCheck    (EqualityCheckContract (..), equalityCheckVerificationBytes)
import           ZkFold.Cardano.UPLC.ForwardingScripts    (forwardingMintCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)

data TransactionInit = TransactionInit
    { curPath         :: !FilePath
    , pathCoreCfg     :: !FilePath
    , txIn            :: !TxIn
    , requiredSigners :: !WitnessSigningData
    , changeAddresses :: !AddressAny
    , outAddress      :: !AddressAny
    , outFile         :: !FilePath
    }

type ScriptName = String

fromRight' :: Either l r -> r
fromRight' (Right x) = x
fromRight' _         = error "fromRight', given a Left"

-- | Sending a compiled script to the network.
sendScript ::
  GYNetworkId ->
  GYProviders ->
  GYPaymentSigningKey ->
  GYAddress ->
  GYTxIn PlutusV3 ->
  GYAddress ->
  GYScript 'PlutusV3 ->
  ScriptName ->
  FilePath ->
  IO ()
sendScript nid providers skey changeAddr txIn sendTo validator name _outFile = do
    pkh <- addressToPubKeyHashIO changeAddr
    let w1 = User' skey Nothing changeAddr
        validator' = Just $ GYPlutusScript validator
        outMin = GYTxOut sendTo (valueFromLovelace 0) Nothing validator'

    params <- gyGetProtocolParameters providers
    let calculateMin = valueFromLovelace $ toInteger $ minimumUTxO params outMin

    let skeleton = mustHaveInput txIn
                <> mustHaveOutput (GYTxOut sendTo calculateMin Nothing validator')
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    print $ name ++ " transaction id: " ++ show txid

tokenInit :: TransactionInit -> IO ()
tokenInit (TransactionInit path pathCfg txIn sig changeAddr outAddress outFile) = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary

    let contract = EqualityCheckContract x ps targetValue

    let testData = path </> "test-data"

    createDirectoryIfMissing True testData

    BL.writeFile (testData </> "plonkup-raw-contract-data.json") $ encode contract

    let (setup, _, _) = equalityCheckVerificationBytes x ps targetValue

    let fmLabel = 0  -- Use a different label (number) to get another 'forwardingMint' address

    coreCfg <- coreConfigIO pathCfg

    (APaymentSigningWitness sks) <- fromRight' <$> readWitnessSigningData sig

    let nid         = cfgNetworkId coreCfg
        skey        = signingKeyFromApi sks
        changeAddr' = addressFromApi changeAddr
        txIn'       = GYTxIn (txOutRefFromApi txIn) GYTxInWitnessKey
        sendTo      = addressFromApi outAddress
        plonkupVerifierToken = validatorFromPlutus $ plonkupVerifierTokenCompiled setup
        forwardingMint       = validatorFromPlutus $ forwardingMintCompiled fmLabel

    withCfgProviders coreCfg "main" $ \providers -> do
      sendScript nid providers skey changeAddr' txIn' sendTo plonkupVerifierToken "plonkupVerifierToken" outFile
      sendScript nid providers skey changeAddr' txIn' sendTo forwardingMint "forwardingMint" outFile
