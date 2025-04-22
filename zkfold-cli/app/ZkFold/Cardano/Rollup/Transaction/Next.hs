module ZkFold.Cardano.Rollup.Transaction.Next (Transaction(..), rollupNext) where

import           Cardano.Api                                 (AddressAny, getScriptData, prettyPrintJSON)
import           Cardano.Api.Shelley                         (TxIn, scriptDataFromJsonDetailedSchema, toPlutusData)
import           Cardano.CLI.Read                            (SomeSigningWitness (..), readWitnessSigningData)
import           Cardano.CLI.Type.Common
import           Control.Monad                               (Functor (..), Monad (..), mapM)
import           Data.Aeson                                  (decode, decodeFileStrict, encodeFile)
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BL
import           Data.List.NonEmpty                          (NonEmpty)
import           Data.Maybe                                  (fromJust)
import           Data.Semigroup                              (Semigroup (..))
import           Data.String                                 (IsString (..))
import           GeniusYield.GYConfig
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V1.Value                    (lovelaceValue)
import           PlutusLedgerApi.V3                          (Address (..), BuiltinByteString, Datum (..),
                                                              OutputDatum (..), ToData (..), TokenName (..), TxOut (..),
                                                              fromData, toBuiltin, toData)
import           Prelude                                     (Either (..), FilePath, IO, Integer, Maybe (..), error,
                                                              undefined, ($), (.), (<$>))
import           System.FilePath                             ((</>))
import           Test.QuickCheck.Arbitrary                   (Arbitrary (..))
import           Test.QuickCheck.Gen                         (generate)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Cardano.Examples.IdentityCircuit     (IdentityCircuitContract (..), stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils               (credentialOf, dataToJSON)
import           ZkFold.Cardano.OnChain.BLS12_381            (toInput)
import           ZkFold.Cardano.OnChain.BLS12_381.F          (F (..))
import           ZkFold.Cardano.OnChain.Utils                (dataToBlake)
import           ZkFold.Cardano.Rollup.Data                  (datumHashEx1, evolve, minReq, rollupFee)
import           ZkFold.Cardano.UPLC.Common                  (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.Rollup                  (RollupInfo (..), RollupRedeemer (..), rollupCompiled)
import           ZkFold.Cardano.UPLC.RollupData              (rollupDataCompiled)

data Transaction = Transaction
    { curPath          :: !FilePath
    , pathCoreCfg      :: !FilePath
    , txIn1            :: !TxIn
    , txInR            :: !TxIn
    , txInRollup       :: !TxIn
    , txidRollupScript :: !FilePath
    , requiredSigners  :: !WitnessSigningData
    , changeAddresses  :: !AddressAny
    , outFile          :: !FilePath
    , dataRefs         :: !(NonEmpty GYTxOutRef)
    , bobAddress       :: !AddressAny
    }

bridgeOut ::
    GYNetworkId ->
    GYProviders ->
    GYPaymentSigningKey ->
    GYAddress ->
    GYAddress ->
    GYTxIn PlutusV3 ->
    GYTxIn PlutusV3 ->
    GYTxOutRef ->
    GYTxId ->
    GYScript PlutusV3 ->
    GYScript PlutusV3 ->
    GYScript PlutusV3 ->
    GYTokenName ->
    Datum ->
    GYRedeemer ->
    NonEmpty GYTxOutRef ->
    FilePath ->
    IO ()
bridgeOut nid providers skey changeAddr bobAddr txIn1 txInR rollupIn txidRollupScript rollup rollupData parkingSpot tokenName state redeemer dataRefs outFile = do
    let w1 = User' skey Nothing changeAddr

        inlineDatum = Just (datumFromPlutusData state, GYTxOutUseInlineDatum @PlutusV3)

        rollupRef  = txOutRefFromTuple (txidRollupScript, 0)
        forwardRef = GYBuildPlutusScriptReference @PlutusV3 rollupRef rollup

        inlineDatum' = Just $ datumFromPlutusData unitDatum
        witness      = GYTxInWitnessScript forwardRef inlineDatum' redeemer

        parkingSpotAddr = addressFromValidator nid parkingSpot
        rollupAddr      = addressFromValidator nid rollup
        token  = GYToken (mintingPolicyId rollupData) tokenName
        tokens = sconcat $ fmap (mustHaveRefInput @PlutusV3) dataRefs

    -- --tx-in-collateral $collateral \
    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = mustHaveInput txIn1
                <> mustHaveInput txInR
                <> tokens
                <> mustHaveInput (GYTxIn @PlutusV3 rollupIn witness)
                <> mustHaveOutput (GYTxOut rollupAddr (valueSingleton token 1 <> valueFromLovelace 3000000) inlineDatum Nothing)
                <> mustHaveOutput (GYTxOut bobAddr (valueFromLovelace 15000000) Nothing Nothing)
                <> mustHaveOutput (GYTxOut parkingSpotAddr (valueFromLovelace 995610) Nothing Nothing)
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    encodeFile outFile txid

-- | Compute next rollup info
nextRollup :: Fr -> Integer -> RollupInfo -> IO RollupInfo
nextRollup x parkingTag rollupInfo = do
    ps <- generate arbitrary

    let dataUpdate1 = riDataUpdate rollupInfo
        state1      = riState      rollupInfo

    dataUpdate2 <- mapM evolve dataUpdate1

    let bridgeTxOut = TxOut { txOutAddress         = Address (credentialOf $ parkingSpotCompiled parkingTag) Nothing
                            , txOutValue           = lovelaceValue minReq
                            , txOutDatum           = OutputDatumHash datumHashEx1
                            , txOutReferenceScript = Nothing
                            }

    let update2 = dataToBlake <$> dataUpdate2
        state2  = toInput $ dataToBlake (state1, update2, [bridgeTxOut], lovelaceValue rollupFee)

        (_, _, proof2)  = stateCheckVerificationBytes x ps state2
        rollupRedeemer2 = UpdateRollup proof2 update2

    return $ RollupInfo parkingTag dataUpdate2 state2 rollupRedeemer2

rollupNext :: Transaction -> IO ()
rollupNext (Transaction path pathCfg txIn1 txInR txInRollup txidRollupScriptFile sig changeAddr outFile dataRefs bobAddress) = do
    let testData = path </> "test-data"
        assets   = path </> "assets"

    IdentityCircuitContract x _ <- fromJust . decode <$> BL.readFile (testData </> "plonk-raw-contract-data.json")

    rollupInfoE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assets </> "rollupInfo.json")

    let rollupInfoScriptData = case rollupInfoE of
          Right a -> a
          Left _  -> error "JSON error: unreadable 'rollupInfo.json'"

    let rollupInfo = fromJust . fromData . toPlutusData . getScriptData $ rollupInfoScriptData :: RollupInfo
    let RollupInfo parkingTag _ (F nextState') rollupRedeemer = rollupInfo

    newRollupInfo <- nextRollup x parkingTag rollupInfo

    BS.writeFile (assets </> "newRollupInfo.json") $ prettyPrintJSON $ dataToJSON newRollupInfo

    ---------------------------
    coreCfg <- coreConfigIO pathCfg
    (Right (APaymentSigningWitness sks)) <- readWitnessSigningData sig
    (Just txidRollupScript) <- decodeFileStrict txidRollupScriptFile

    let nid         = cfgNetworkId coreCfg
        skey        = signingKeyFromApi sks
        changeAddr' = addressFromApi changeAddr
        txIn1'      = GYTxIn (txOutRefFromApi txIn1) GYTxInWitnessKey
        txInR'      = GYTxIn (txOutRefFromApi txInR) GYTxInWitnessKey
        rollupIn    = txOutRefFromApi txInRollup
        rollupData  = validatorFromPlutus rollupDataCompiled
        parkingSpot = validatorFromPlutus $ parkingSpotCompiled parkingTag
        tokenName'  = tokenNameFromPlutus $ TokenName (fromString "zkFold" :: BuiltinByteString)
        bobAddr     = addressFromApi bobAddress
        rollupSetup = undefined
        rollup      = validatorFromPlutus $ rollupCompiled rollupSetup
        state       = Datum $ toBuiltin $ toData nextState'
        redeemer    = redeemerFromPlutus' $ toBuiltinData rollupRedeemer

    let tokenName = case tokenName' of
            Just a  -> a
            Nothing -> error "invalid token name"

    withCfgProviders coreCfg "main" $ \providers -> do
        bridgeOut nid providers skey changeAddr' bobAddr txIn1' txInR' rollupIn txidRollupScript rollup rollupData parkingSpot tokenName state redeemer dataRefs outFile
