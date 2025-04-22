module ZkFold.Cardano.Rollup.Transaction.Update (Transaction(..), rollupUpdate) where

import           Cardano.Api                    (AddressAny, TxIn, getScriptData)
import           Cardano.Api.Shelley            (scriptDataFromJsonDetailedSchema, toPlutusData)
import           Cardano.CLI.Read               (SomeSigningWitness (..), readWitnessSigningData)
import           Cardano.CLI.Type.Common        (WitnessSigningData)
import           Data.Aeson                     (decode, encodeFile)
import qualified Data.ByteString.Lazy           as BL
import           Data.Maybe                     (fromJust)
import           GeniusYield.GYConfig           (GYCoreConfig (cfgNetworkId), coreConfigIO, withCfgProviders)
import           GeniusYield.Transaction.Common (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3             (ToData (..), TokenName (..), fromData)
import           Prelude                        (Either (..), FilePath, IO, Integer, Integral (..), Maybe (..),
                                                 Num (fromInteger), Semigroup (..), error, (!!), ($), (.), (<$>))
import           System.FilePath                ((</>))

import           ZkFold.Cardano.OnChain.Utils   (dataToBlake)
import           ZkFold.Cardano.UPLC.Common     (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.Rollup     (RollupInfo (..))
import           ZkFold.Cardano.UPLC.RollupData (RollupDataRedeemer (..), rollupDataCompiled)

data Transaction = Transaction
    { curPath         :: !FilePath
    , pathCoreCfg     :: !FilePath
    , txIn            :: !TxIn
    , requiredSigners :: !WitnessSigningData
    , changeAddresses :: !AddressAny
    , outFile         :: !FilePath
    , index           :: !Integer
    }

tokenUpdate ::
    GYNetworkId ->
    GYProviders ->
    GYPaymentSigningKey ->
    GYAddress ->
    GYTxIn PlutusV3 ->
    GYScript PlutusV3 ->
    GYScript PlutusV3 ->
    GYTokenName ->
    GYRedeemer ->
    FilePath ->
    IO ()
tokenUpdate nid providers skey changeAddr txIn rollupData parkingSpot tokenName redeemer outFile = do
    let w1 = User' skey Nothing changeAddr

        inlineDatum = Just (unitDatum, GYTxOutUseInlineDatum @PlutusV3)
        script      = GYBuildPlutusScript $ GYBuildPlutusScriptInlined rollupData

        parkingSpotAddr = addressFromValidator nid parkingSpot
        token           = GYToken (mintingPolicyId rollupData) tokenName
        outMin          = GYTxOut parkingSpotAddr (valueSingleton token 1) inlineDatum Nothing

    params <- gyGetProtocolParameters providers
    let calculateMin = valueFromLovelace $ toInteger $ minimumUTxO params outMin

    -- --tx-in-collateral $collateral \
    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = mustHaveInput txIn
                <> mustHaveOutput (GYTxOut parkingSpotAddr calculateMin inlineDatum Nothing)
                <> mustMint script redeemer tokenName 1
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    encodeFile outFile txid

rollupUpdate :: Transaction -> IO ()
rollupUpdate (Transaction path pathCfg txIn sig changeAddr outFile index) = do
    let assets   = path </> "assets"

    rollupInfoE <- scriptDataFromJsonDetailedSchema . fromJust . decode <$> BL.readFile (assets </> "rollupInfo.json")

    let rollupInfoScriptData = case rollupInfoE of
          Right a -> a
          Left _  -> error "JSON error: unreadable 'rollupInfo.json'"

    let rollupInfo = fromJust . fromData . toPlutusData . getScriptData $ rollupInfoScriptData :: RollupInfo
    let RollupInfo parkingTag dataUpdate _ _ = rollupInfo

    coreCfg <- coreConfigIO pathCfg
    (Right (APaymentSigningWitness sks)) <- readWitnessSigningData sig

    let nid         = cfgNetworkId coreCfg
        skey        = signingKeyFromApi sks
        changeAddr' = addressFromApi changeAddr
        txIn'       = GYTxIn (txOutRefFromApi txIn) GYTxInWitnessKey
        rollupData  = validatorFromPlutus rollupDataCompiled
        parkingSpot = validatorFromPlutus $ parkingSpotCompiled parkingTag
        tokenName'  = tokenNameFromPlutus $ TokenName $ dataToBlake $ dataUpdate !! fromInteger index
        redeemer    = redeemerFromPlutus' $ toBuiltinData $ NewData $ dataUpdate !! fromInteger index

    let tokenName = case tokenName' of
            Just a  -> a
            Nothing -> error "invalid token name"

    withCfgProviders coreCfg "main" $ \providers -> do
        tokenUpdate nid providers skey changeAddr' txIn' rollupData parkingSpot tokenName redeemer outFile
