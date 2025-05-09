module ZkFold.Cardano.PlonkupVerifierToken.Transaction.Burning (tokenBurning, Transaction(..)) where

import           Control.Exception                        (throwIO)
import           Data.Aeson                               (decode)
import qualified Data.ByteString.Lazy                     as BL
import           Data.Maybe                               (fromJust)
import           GeniusYield.GYConfig                     (GYCoreConfig (..), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3                       (ToData (..))
import           Prelude
import           System.FilePath                          ((</>))

import           ZkFold.Cardano.Atlas.Utils               (SubmittedTx (..), wrapUpSubmittedTx)
import           ZkFold.Cardano.Examples.EqualityCheck    (EqualityCheckContract (..), equalityCheckVerificationBytes)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F       as F
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes (..))
import           ZkFold.Cardano.Options.Common            (CoreConfigAlt, SigningKeyAlt, TokenAlt, TxIdAlt,
                                                           fromCoreConfigAltIO, fromSigningKeyAltIO, fromTokenAltIO,
                                                           fromTxIdAltIO)
import           ZkFold.Cardano.UPLC.ForwardingScripts    (forwardingMintCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)

data Transaction = Transaction
    { curPath        :: !FilePath
    , coreCfg        :: !CoreConfigAlt
    , fmTag          :: !Integer
    , requiredSigner :: !SigningKeyAlt
    , changeAddress  :: !GYAddress
    , token          :: !TokenAlt
    , txidSetup      :: !TxIdAlt
    , outFile        :: !FilePath
    }

-- | burining tokens to the reward.
burnTokens :: GYNetworkId         ->
              GYProviders         ->
              GYPaymentSigningKey ->
              -- ^ Signing key for wallet funding this Tx.
              GYAddress           ->
              -- ^ Change address for wallet funding this Tx.
              GYScript PlutusV3   ->
              -- ^ Parameterized PlonkupVerifierToken script.
              GYScript PlutusV3   ->
              -- ^ Parameterized ForwardingMint script.
              GYAssetClass        ->
              -- ^ Token to burn.
              GYTxId              ->
              -- ^ Setup reference TxId.
              FilePath            ->
              -- ^ Path to output file.
              IO ()
burnTokens nid providers skey changeAddr plonkupVerifierToken forwardingMint token txidSetup outFile = do
  let w1     = User' skey Nothing changeAddr
      fmAddr = addressFromValidator nid forwardingMint

  let txOutRefPlonkup = txOutRefFromTuple (txidSetup, 0)
      txOutRefFM      = txOutRefFromTuple (txidSetup, 1)

      plonkupRef = GYBuildPlutusScriptReference @PlutusV3 txOutRefPlonkup plonkupVerifierToken
      fmRef      = GYBuildPlutusScriptReference @PlutusV3 txOutRefFM forwardingMint

  case token of
    GYToken pid tn -> do
      let cs          = mintingPolicyIdToCurrencySymbol pid
          inlineDatum = GYOutDatumInline $ datumFromPlutusData cs

      utxosAtFM <- runGYTxQueryMonadIO nid
                                       providers
                                       (utxosAtAddress fmAddr Nothing)

      let utxosAtFM' = utxosToList $ filterUTxOs (\u -> utxoOutDatum u == inlineDatum) utxosAtFM

      case utxosAtFM' of
        utxo : _ -> do
          let dummyRedeemer' = ProofBytes "" "" "" "" "" "" "" "" "" "" "" "" "" 0 0 0 0 0 0 0 0 0 0 0 0 (F.F 0) [F.F 0]
              dummyRedeemer  = redeemerFromPlutusData $ toBuiltinData dummyRedeemer'

          let fmWit = GYTxInWitnessScript fmRef Nothing unitRedeemer

          pkh <- addressToPubKeyHashIO changeAddr

          let skeleton = mustHaveInput (GYTxIn @PlutusV3 (utxoRef utxo) fmWit)
                      <> mustMint (GYBuildPlutusScript plonkupRef) dummyRedeemer tn (-1)
                      <> mustBeSignedBy pkh

          tx <- runGYTxGameMonadIO nid
                                   providers $
                                   asUser w1 $ do
                                     ownAddrs <- ownAddresses
                                     let skeleton' = skeleton <> mustHaveOutput (GYTxOut (head ownAddrs) (utxoValue utxo) Nothing Nothing)
                                     txbody <- buildTxBody skeleton'
                                     txid   <- signAndSubmitConfirmed txbody
                                     return $ SubmittedTx txid (Just $ txBodyFee txbody)

          wrapUpSubmittedTx outFile tx

        _ -> throwIO $ userError "No UTxO with expected datum found."
    _ -> throwIO $ userError "Missing native token."

tokenBurning :: Transaction -> IO ()
tokenBurning (Transaction path coreCfg' tag sig changeAddr token' txidSetup' outFile) = do
  let assetsPath = path </> "assets"
      testData   = path </> "test-data"

  coreCfg   <- fromCoreConfigAltIO coreCfg'
  skey      <- fromSigningKeyAltIO sig
  token     <- fromTokenAltIO assetsPath token'
  txidSetup <- fromTxIdAltIO assetsPath txidSetup'

  let nid = cfgNetworkId coreCfg

  EqualityCheckContract{..} <- fromJust . decode <$> BL.readFile (testData </> "plonkup-raw-contract-data.json")

  let (setup, _, _)        = equalityCheckVerificationBytes x ps targetValue
      plonkupVerifierToken = scriptFromPlutus @PlutusV3 $ plonkupVerifierTokenCompiled setup

      forwardingMint = scriptFromPlutus @PlutusV3 $ forwardingMintCompiled tag

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> burnTokens
                                                          nid
                                                          providers
                                                          skey
                                                          changeAddr
                                                          plonkupVerifierToken
                                                          forwardingMint
                                                          token
                                                          txidSetup
                                                          (assetsPath </> outFile)
