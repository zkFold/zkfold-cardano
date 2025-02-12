module ZkFold.Cardano.PlonkupVerifierToken.Transaction.Init (tokenInit) where


import           Data.Aeson                               (encode)
import qualified Data.ByteString.Lazy                     as BL
import           GeniusYield.Transaction.Common           (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types                        (GYAddress, GYAnyScript (..), GYNetworkId,
                                                           GYPaymentSigningKey, GYProviders, GYScript, GYTxIn,
                                                           GYTxOut (..), PlutusVersion (..), gyGetProtocolParameters,
                                                           valueFromLovelace)
import           GeniusYield.Types.Script                 (validatorFromPlutus)
import           Prelude                                  (Bool (..), FilePath, IO, Maybe (..), Show (..), String,
                                                           print, toInteger, undefined, ($), (++), (<>))
import           System.Directory                         (createDirectoryIfMissing)
import           System.FilePath                          ((</>))
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)

import           ZkFold.Cardano.Examples.EqualityCheck    (EqualityCheckContract (..), equalityCheckVerificationBytes)
import           ZkFold.Cardano.UPLC.ForwardingScripts    (forwardingMintCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)



type ScriptName = String

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
  IO ()
sendScript nid providers skey changeAddr txIn sendTo validator name = do
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

tokenInit :: FilePath -> IO ()
tokenInit path = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary

    let contract = EqualityCheckContract x ps targetValue

    let testData = path </> "test-data"

    createDirectoryIfMissing True testData

    BL.writeFile (testData </> "plonkup-raw-contract-data.json") $ encode contract

    let (setup, _, _) = equalityCheckVerificationBytes x ps targetValue

    let fmLabel = 0  -- Use a different label (number) to get another 'forwardingMint' address

    let nid = undefined
        providers = undefined
        skey = undefined
        changeAddr = undefined
        txIn = undefined
        sendTo = undefined
        plonkupVerifierToken = validatorFromPlutus $ plonkupVerifierTokenCompiled setup
        forwardingMint       = validatorFromPlutus $ forwardingMintCompiled fmLabel

    sendScript nid providers skey changeAddr txIn sendTo plonkupVerifierToken "plonkupVerifierToken"
    sendScript nid providers skey changeAddr txIn sendTo forwardingMint "forwardingMint"
