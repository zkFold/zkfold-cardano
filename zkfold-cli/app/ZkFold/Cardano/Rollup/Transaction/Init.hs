module ZkFold.Cardano.Rollup.Transaction.Init (Transaction(..), rollupInit) where

import           Cardano.Api                             (AddressAny, TxIn, prettyPrintJSON)
import           Cardano.CLI.Read                        (SomeSigningWitness (..), readWitnessSigningData)
import           Cardano.CLI.Type.Common                 (WitnessSigningData)
import           Control.Monad                           (Functor (..), mapM)
import           Data.Aeson                              (encode, encodeFile)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.String                             (IsString (..))
import           GeniusYield.GYConfig                    (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.Transaction.Common          (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types                       (GYAssetClass (..), GYNetworkId, GYProviders, GYTokenName (..),
                                                          GYTxIn (..), GYTxOut (..), GYTxOutUseInlineDatum (..),
                                                          GYValue, PlutusVersion (..), datumFromPlutusData,
                                                          gyGetProtocolParameters, valueFromLovelace, valueSingleton)
import           GeniusYield.Types.Address               (GYAddress, addressFromApi, addressFromValidator,
                                                          addressToPlutus)
import           GeniusYield.Types.BuildScript
import           GeniusYield.Types.Key                   (GYPaymentSigningKey, signingKeyFromApi)
import           GeniusYield.Types.Redeemer              (unitRedeemer)
import           GeniusYield.Types.Script                (GYAnyScript (..), GYScript, mintingPolicyCurrencySymbol,
                                                          mintingPolicyId, validatorFromPlutus, validatorPlutusHash)
import           GeniusYield.Types.TxIn                  (GYTxInWitness (..))
import           GeniusYield.Types.TxOutRef              (txOutRefFromApi, txOutRefToPlutus)
import           GeniusYield.Types.Value                 (tokenNameFromPlutus)
import           PlutusLedgerApi.V1.Value                (lovelaceValue)
import qualified PlutusLedgerApi.V2                      as V2
import           PlutusLedgerApi.V3                      (Address (..), BuiltinByteString, Datum (..), OutputDatum (..),
                                                          TokenName (..), TxOut (..), singleton, toBuiltin, toData)
import qualified PlutusLedgerApi.V3                      as V3
import           Prelude                                 (Either (..), FilePath, IO, Integer, Integral (..), Maybe (..),
                                                          Semigroup (..), error, putStr, ($), (-), (<$>))
import           System.FilePath                         ((</>))
import           System.Random                           (randomRIO)
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (dataToJSON)
import           ZkFold.Cardano.OnChain.BLS12_381        (F (..), bls12_381_field_prime, toInput)
import           ZkFold.Cardano.OnChain.Utils            (dataToBlake)
import           ZkFold.Cardano.Rollup.Data              (datumHashEx1, minReq, rmax, rollupFee, threadLovelace,
                                                          updateLength)
import           ZkFold.Cardano.UPLC.Common              (nftPolicyCompiled, parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.Rollup              (RollupInfo (..), RollupRedeemer (..), RollupSetup (..),
                                                          rollupCompiled)
import           ZkFold.Cardano.UPLC.RollupData          (rollupDataCompiled)

data Transaction = Transaction
    { curPath         :: !FilePath
    , pathCoreCfg     :: !FilePath
    , txIn1           :: !TxIn
    , txIn2           :: !TxIn
    , requiredSigners :: !WitnessSigningData
    , changeAddresses :: !AddressAny
    , outFile         :: !FilePath
    , bobAddress      :: !AddressAny
    , lovelace        :: !Integer
    }

initState ::
    GYNetworkId ->
    GYProviders ->
    GYPaymentSigningKey ->
    GYAddress ->
    GYTxIn PlutusV3 ->
    GYScript PlutusV3 ->
    GYScript PlutusV3 ->
    Datum ->
    GYTokenName ->
    FilePath ->
    IO ()
initState nid providers skey changeAddr txIn1 nftPolicy rollup state tokenName outFile = do
    let w1 = User' skey Nothing changeAddr

        inlineDatum = Just (datumFromPlutusData state, GYTxOutUseInlineDatum @PlutusV3)
        script      = GYBuildPlutusScript $ GYBuildPlutusScriptInlined nftPolicy

        rollupAddr = addressFromValidator nid rollup
        token      = GYToken (mintingPolicyId nftPolicy) tokenName

    --tx-in-collateral $in1 \
    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = mustHaveInput txIn1
                <> mustHaveOutput (GYTxOut rollupAddr (valueSingleton token 1 <> valueFromLovelace 3000000) inlineDatum Nothing)
                <> mustMint script unitRedeemer tokenName 1
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    encodeFile outFile txid

parkRollup ::
    GYNetworkId ->
    GYProviders ->
    GYPaymentSigningKey ->
    GYAddress ->
    GYTxIn PlutusV3 ->
    GYScript PlutusV3 ->
    GYScript PlutusV3 ->
    GYValue ->
    FilePath ->
    IO ()
parkRollup nid providers skey changeAddr txIn2 parkingSpot rollup lovelace outFile = do
    let w1 = User' skey Nothing changeAddr
        parkingSpotAddr = addressFromValidator nid parkingSpot

        validator' = Just $ GYPlutusScript rollup
        outMin = GYTxOut parkingSpotAddr (valueFromLovelace 0) Nothing validator'

    params <- gyGetProtocolParameters providers
    let calculateMin = valueFromLovelace $ toInteger $ minimumUTxO params outMin

    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = mustHaveInput txIn2
                <> mustHaveOutput (GYTxOut parkingSpotAddr calculateMin Nothing Nothing)
                <> mustHaveOutput (GYTxOut changeAddr lovelace Nothing Nothing)
                <> mustHaveOutput (GYTxOut changeAddr lovelace Nothing Nothing)
                <> mustBeSignedBy pkh

    txid <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        signAndSubmitConfirmed txBody

    encodeFile outFile txid

rollupInit :: Transaction -> IO ()
rollupInit (Transaction path pathCfg txIn1 txIn2 sig changeAddr outFile bobAddress lovelace) = do
    parkingTag <- randomRIO (1, 10000)
    x          <- generate arbitrary
    ps         <- generate arbitrary
    seeds      <- mapM (\_ -> randomRIO (1, rmax)) [1..updateLength]
    iniState'  <- randomRIO (0, bls12_381_field_prime - 1)

    let contract   = IdentityCircuitContract x ps
        dataUpdate = fmap (\s -> [dataToBlake s]) seeds
        update     = dataToBlake <$> dataUpdate
        iniState   = F iniState'
        parkingSpot = validatorFromPlutus $ parkingSpotCompiled parkingTag

    let bridgeTxOut = TxOut { txOutAddress         = Address (V3.ScriptCredential $ validatorPlutusHash parkingSpot) Nothing
                            , txOutValue           = lovelaceValue minReq
                            , txOutDatum           = OutputDatumHash datumHashEx1
                            , txOutReferenceScript = Nothing
                            }

        nextState = toInput $ dataToBlake (iniState, update, [bridgeTxOut], lovelaceValue rollupFee)

        (_, _, proof) = stateCheckVerificationBytes x ps nextState

    let rollupRedeemer = UpdateRollup proof update
        rollupInfo     = RollupInfo parkingTag dataUpdate nextState rollupRedeemer

    let testData = path </> "test-data"
        assets   = path </> "assets"
    BL.writeFile (testData </> "plonk-raw-contract-data.json") $ encode contract
    BS.writeFile (assets </> "rollupInfo.json") $ prettyPrintJSON $ dataToJSON rollupInfo

    putStr "\nDone serializing plutus scripts and initializing state.\n\n"

    ---
    let (ledgerRules, _, _) = identityCircuitVerificationBytes x ps
        addr        = addressToPlutus $ addressFromApi bobAddress
        nftOref     = fmapTxOutRef $ txOutRefToPlutus $ txOutRefFromApi txIn1

    let nftPolicy   = validatorFromPlutus $ nftPolicyCompiled nftOref
        threadCS    = mintingPolicyCurrencySymbol nftPolicy
        threadName  = TokenName (fromString "zkFold" :: BuiltinByteString)
        rollupSetup = RollupSetup
                      { rsLedgerRules  = ledgerRules
                      , rsDataCurrency = mintingPolicyCurrencySymbol $ validatorFromPlutus @PlutusV3 rollupDataCompiled
                      , rsThreadValue  = lovelaceValue threadLovelace <> singleton threadCS threadName 1
                      , rsFeeAddress   = addr
                      }
    coreCfg <- coreConfigIO pathCfg
    (Right (APaymentSigningWitness sks)) <- readWitnessSigningData sig

    let nid         = cfgNetworkId coreCfg
        skey        = signingKeyFromApi sks
        changeAddr' = addressFromApi changeAddr
        txIn1'      = GYTxIn (txOutRefFromApi txIn1) GYTxInWitnessKey
        txIn2'      = GYTxIn (txOutRefFromApi txIn2) GYTxInWitnessKey
        rollup      = validatorFromPlutus $ rollupCompiled rollupSetup

        state      = Datum $ toBuiltin $ toData iniState'
        tokenName' = tokenNameFromPlutus threadName
        lovelace'  = valueFromLovelace lovelace

    let tokenName = case tokenName' of
            Just a  -> a
            Nothing -> error "invalid token name"

    withCfgProviders coreCfg "main" $ \providers -> do
        initState nid providers skey changeAddr' txIn1' nftPolicy rollup state tokenName outFile
        parkRollup nid providers skey changeAddr' txIn2' parkingSpot rollup lovelace' outFile

----- HELPER FUNCTIONS -----

fmapTxId :: V2.TxId -> V3.TxId
fmapTxId (V2.TxId a) = V3.TxId a

fmapTxOutRef :: V2.TxOutRef -> V3.TxOutRef
fmapTxOutRef (V2.TxOutRef a b) = V3.TxOutRef (fmapTxId a) b
