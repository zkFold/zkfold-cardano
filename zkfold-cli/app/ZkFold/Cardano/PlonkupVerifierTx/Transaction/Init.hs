module ZkFold.Cardano.PlonkupVerifierTx.Transaction.Init where

-- import           Cardano.Api                           (SerialiseAsRawBytes (..), parsePolicyId, prettyPrintJSON, runExceptT)
-- import           Cardano.Api.Ledger                    (toCBOR)
-- import           Cardano.Api.Shelley                   (toPlutusData)
-- import           Cardano.CLI.Type.Common               (ReferenceScriptAnyEra (..), ScriptDataOrFile, TxOutAnyEra (..), TxOutDatumAnyEra (..))
-- import           Cardano.CLI.EraBased.Script.Read.Common (readScriptDataOrFile)
-- import           Codec.CBOR.Write                      (toStrictByteString)
import           Control.Exception                     (throwIO)
-- import           Control.Monad                         (forM, when)
import           Data.Aeson                            (decode, encode)
-- import           Data.ByteString                       as BS (writeFile)
import qualified Data.ByteString.Lazy                  as BL
-- import           Data.Coerce                           (coerce)
import           Data.Maybe                            (fromJust, isJust)
import           GeniusYield.GYConfig                  (GYCoreConfig (cfgNetworkId), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
-- import           PlutusLedgerApi.V1                    (TxOutRef (..))
-- import qualified PlutusLedgerApi.V2                    as V2
-- import           PlutusLedgerApi.V3                    as V3
-- import           PlutusLedgerApi.V3                    (BuiltinData, OutputDatum (..), ScriptHash, ToData, TxInInfo (..), TxOut (..), TxOutRef (..), UnsafeFromData,
--                                                         toBuiltinData, unsafeFromBuiltinData, unsafeFromData)
-- import qualified PlutusTx.Builtins.Internal            as BI
-- import           PlutusTx.Prelude                      (blake2b_224, sortBy)
import           Prelude
-- import           Prelude                               (Bool (..), Either (..), FilePath, IO, Show (..), String, error,
--                                                         head, ($), (++), (.), (<$>))
import           System.Directory                      (createDirectoryIfMissing, doesFileExist)
import           System.FilePath                       ((</>))
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)
-- import           Text.Parsec                           (parse)

import           ZkFold.Cardano.Examples.EqualityCheck (EqualityCheckContract (..), equalityCheckVerificationBytes)
--import           ZkFold.Cardano.OffChain.Utils         (dataToJSON, outRefCompare, savePlutus)
-- import           ZkFold.Cardano.OnChain.BLS12_381      (toInput)
import           ZkFold.Cardano.Options.Common
import           ZkFold.Cardano.PlonkupVerifierTx.Types
-- import           ZkFold.Cardano.Options.Common         (CoreConfigAlt, SigningKeyAlt, TxInputInfo (..), fromCoreConfigAltIO,
--                                                         fromSigningKeyAltIO)
-- import           ZkFold.Cardano.UPLC.ForwardingScripts (forwardingRewardCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx (plonkupVerifierTxCompiled)


data Transaction = Transaction
  { curPath    :: !FilePath
  , coreCfgAlt :: !CoreConfigAlt
  }

verifierInit :: Transaction -> IO ()
verifierInit (Transaction path coreCfg') = do
  let assetsPath = path </> "assets"
      setupFile  = assetsPath </> "plonkupVerifierTx-setup-data.json"

  createDirectoryIfMissing True assetsPath

  setupFileExists <- doesFileExist setupFile
  coreCfg         <- fromCoreConfigAltIO coreCfg'

  let nid = cfgNetworkId coreCfg

  if setupFileExists
    then do
      PlonkupVerifierTxSetup x <- fromJust . decode <$> BL.readFile setupFile
      ps                       <- generate arbitrary
      targetValue              <- generate arbitrary

      let (setup, _,_ )   = equalityCheckVerificationBytes x ps targetValue
          plutusValidator = plonkupVerifierTxCompiled setup
          script          = scriptFromPlutus @PlutusV3 plutusValidator
          scriptAddr      = addressFromValidator nid script

      putStr "\nplonkupVerifierTx Address:\n\n"
      print scriptAddr
    else do
      return ()

{-
verifierTx :: Transaction -> IO ()
verifierTx (Transaction coreCfg' _inVerify ins gyRefs outs) = do
  coreCfg  <- fromCoreConfigAltIO coreCfg'

  let nid = cfgNetworkId coreCfg

  withCfgProviders coreCfg "zkfold-cli" $ \providers -> do
    gyTxIns  <- forM ins txInFromInfo
    gyTxOuts <- forM outs txOutFromApi

    let skeleton = mconcat (mustHaveInput <$> gyTxIns)
                <> mconcat (mustHaveRefInput <$> gyRefs)
                <> mconcat (mustHaveOutput <$> gyTxOuts)

    mUtxosIn  <- forM (txinOref <$> ins) $ runGYTxQueryMonadIO nid providers . utxoAtTxOutRef
    mUtxosRef <- forM gyRefs $ runGYTxQueryMonadIO nid providers . utxoAtTxOutRef

    when (not $ all isJust mUtxosIn) . throwIO $ userError "No UTxO found for some input."
    when (not $ all isJust mUtxosRef) . throwIO $ userError "No UTxO found for some reference input."

    let ins'  = sortTxInInfoList $ utxoToTxInInfo . fromJust <$> mUtxosIn
        refs' = sortTxInInfoList $ utxoToTxInInfo . fromJust <$> mUtxosRef
        outs' = txOutToPlutus <$> gyTxOuts

    let insBD    = toBuiltinData ins'
        refsBD   = toBuiltinData refs'
        outsBD   = toBuiltinData outs'
        rangeBD  = toBuiltinData alwaysValid  -- ToDo: generalize to bounded intervals
        txDataBD = mkTuple4 insBD refsBD outsBD rangeBD

    let input = toInput . blake2b_224 . BI.serialiseData $ txDataBD

    putStr $ "Verifier's input: " ++ (show input) ++ "\n\n"

-}

----- HELPER FUNCTIONS -----

