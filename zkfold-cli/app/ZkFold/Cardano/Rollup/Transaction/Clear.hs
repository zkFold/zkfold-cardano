module ZkFold.Cardano.Rollup.Transaction.Clear (Transaction(..), rollupClear) where

import           Cardano.Api                    (getScriptData)
import           Cardano.Api.Shelley            (scriptDataFromJsonDetailedSchema, toPlutusData)
import           Control.Exception              (throwIO)
import           Data.Aeson                     (decode, decodeFileStrict)
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust)
import           GeniusYield.GYConfig           (GYCoreConfig (cfgNetworkId), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3             (Redeemer (..), TokenName (..), Value (..), dataToBuiltinData, fromData,
                                                 toData)
import           PlutusTx.AssocMap              as Pam
import           Prelude
import           System.FilePath                ((</>))

import           ZkFold.Cardano.Options.Common  (CoreConfigAlt, SigningKeyAlt, SubmittedTx (..), fromCoreConfigAltIO,
                                                 fromSigningKeyAltIO, wrapUpSubmittedTx)
import           ZkFold.Cardano.UPLC.Common     (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.Rollup     (RollupInfo (..))
import           ZkFold.Cardano.UPLC.RollupData (RollupDataRedeemer (..), rollupDataCompiled)


data Transaction = Transaction
  { curPath        :: !FilePath
  , coreCfgAlt     :: !CoreConfigAlt
  , requiredSigner :: !SigningKeyAlt
  , changeAddress  :: !GYAddress
  , initOutFile    :: !FilePath
  , outFile        :: !FilePath
  }

dataTokens :: GYMintingPolicyId -> GYUTxO -> Maybe [(TokenName, Integer)]
dataTokens pid utxo = fmap Pam.toList . Pam.lookup (mintingPolicyIdToCurrencySymbol pid)
                      . getValue . valueToPlutus $ utxoValue utxo

mustBurnDataToken :: GYBuildPlutusScript PlutusV3 -> (TokenName, Integer) -> GYTxSkeleton PlutusV3
mustBurnDataToken dataRef (tn, n) = mustMint (GYBuildPlutusScript dataRef) burnRedeemer gytn (-n)
  where
    burnRedeemer = redeemerFromPlutus . Redeemer . dataToBuiltinData $ toData OldData
    gytn         = fromJust $ tokenNameFromPlutus tn

burnDataTokens :: Integer                      ->
                  GYBuildPlutusScript PlutusV3 ->
                  GYTxOutRef                   ->
                  [(TokenName, Integer)]       ->
                  GYTxSkeleton PlutusV3
burnDataTokens parkingTag dataRef oref tks =
  let parkingSpot = scriptFromPlutus @PlutusV3 $ parkingSpotCompiled parkingTag
      parkingWit  = GYTxInWitnessScript (GYBuildPlutusScriptInlined parkingSpot) Nothing unitRedeemer

  in   mustHaveInput (GYTxIn oref parkingWit)
    <> mconcat (mustBurnDataToken dataRef <$> tks)

rollupClear :: Transaction -> IO ()
rollupClear (Transaction path coreCfg' sig changeAddr initOut outFile) = do
  let assets = path </> "assets"

  initTxId <- decodeFileStrict (assets </> initOut)
              >>= maybe (fail $ "Failed to decode " ++ initOut) pure

  let rollupData   = scriptFromPlutus @PlutusV3 $ rollupDataCompiled
      dataPolicyId = mintingPolicyId rollupData

  rollupInfoE  <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assets </> "rollupInfo.json")

  case rollupInfoE of
    Right rollupInfoScriptData -> do
      coreCfg  <- fromCoreConfigAltIO coreCfg'
      skey     <- fromSigningKeyAltIO sig

      let nid = cfgNetworkId coreCfg

      let rollupInfo  = fromJust . fromData . toPlutusData . getScriptData $ rollupInfoScriptData :: RollupInfo
          RollupInfo parkingTag _ _ _ = rollupInfo
          parkingSpot = scriptFromPlutus @PlutusV3 $ parkingSpotCompiled parkingTag
          parkingAddr = addressFromValidator nid parkingSpot

      let dataRefOref = txOutRefFromTuple (initTxId, 1)
          dataRef     = GYBuildPlutusScriptReference @PlutusV3 dataRefOref rollupData

      pkh <- addressToPubKeyHashIO changeAddr

      let w1 = User' skey Nothing changeAddr

      withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
        utxos <- runGYTxQueryMonadIO nid
                                     providers
                                     (utxosAtAddress parkingAddr Nothing)

        let dataTuples = Map.toList $ mapMaybeUTxOs (dataTokens dataPolicyId) utxos
            skeleton'  = mconcat $ uncurry (burnDataTokens parkingTag dataRef) <$> dataTuples
            skeleton   = skeleton' <> mustBeSignedBy pkh

        tx <- runGYTxGameMonadIO nid
                                 providers $
                                 asUser w1 $ do
                                   txbody <- buildTxBody $ skeleton
                                   txid   <- signAndSubmitConfirmed txbody
                                   return $ SubmittedTx txid (Just $ txBodyFee txbody)

        wrapUpSubmittedTx (assets </> outFile) tx

    Left _  -> throwIO $ userError  "JSON error: unreadable rollupInfo script data."
