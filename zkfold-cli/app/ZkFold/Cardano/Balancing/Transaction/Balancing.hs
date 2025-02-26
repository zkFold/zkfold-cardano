module ZkFold.Cardano.Balancing.Transaction.Balancing where

import           Cardano.Api                             (AddressAny, TxIn)
import           Cardano.CLI.Read                        (SomeSigningWitness (..), readWitnessSigningData)
import           Cardano.CLI.Types.Common                (WitnessSigningData)
import           Data.Aeson                              (decode, decodeFileStrict, encodeFile)
import qualified Data.ByteString.Lazy                    as BL
import           Data.Maybe                              (fromJust)
import           GeniusYield.GYConfig                    (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V1.Value                (lovelaceValue)
import qualified PlutusLedgerApi.V2                      as V2
import qualified PlutusLedgerApi.V2.Tx                   as Plutus
import           PlutusLedgerApi.V3                      (BuiltinData, Datum (..), Extended (..), Interval (..),
                                                          Lovelace (..), LowerBound (..), OutputDatum (..), POSIXTime,
                                                          ToData (..), TxInInfo (..), TxOut (..), UpperBound (..),
                                                          toBuiltin, toData)
import qualified PlutusLedgerApi.V3                      as Plutus
import qualified PlutusLedgerApi.V3                      as V3
import qualified PlutusTx.Builtins.Internal              as BI
import           PlutusTx.Prelude                        (Functor (..), blake2b_224, sortBy)
import           Prelude                                 (Bool (..), Either (..), FilePath, IO, Maybe (..),
                                                          Semigroup (..), Show (..), putStr, ($), (++), (.), (<$>))
import           System.FilePath                         ((</>))

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (outRefCompare)
import           ZkFold.Cardano.OnChain.BLS12_381        (toInput)
import           ZkFold.Cardano.UPLC.Common              (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx   (plonkupVerifierTxCompiled)

data Transaction = Transaction
    { curPath            :: !FilePath
    , pathCoreCfg        :: !FilePath
    , txIn1              :: !TxIn
    , txIn2              :: !TxIn
    , requiredSigners    :: !WitnessSigningData
    , changeAddresses    :: !AddressAny
    , outFile            :: !FilePath
    , txidverifierTxFile :: !FilePath
    , utxo1Addr          :: !AddressAny
    }

calculateBalancing ::
    GYNetworkId ->
    GYProviders ->
    GYPaymentSigningKey ->
    GYAddress ->
    GYTxIn PlutusV3 ->
    GYTxIn PlutusV3 ->
    GYAddress ->
    GYTxOutRef ->
    GYScript PlutusV3 ->
    GYTxId ->
    Datum ->
    GYRedeemer ->
    FilePath ->
    IO ()
calculateBalancing nid providers skey changeAddr txIn1 txIn2 sendTo verifierTx verifierTxScript txidTx datum redeemer outFile = do
    let w1 = User' skey Nothing changeAddr

        verifierTxRef = txOutRefFromTuple (txidTx, 0)
        forwardRef = GYBuildPlutusScriptReference @PlutusV3 verifierTxRef (validatorToScript verifierTxScript)

        inlineDatum = Just $ datumFromPlutusData datum
        witness = GYTxInWitnessScript forwardRef inlineDatum redeemer

    pkh <- addressToPubKeyHashIO changeAddr
    let skeleton = mustHaveInput txIn1
                <> mustHaveInput txIn2
                <> mustHaveInput (GYTxIn @PlutusV3 verifierTx witness)
                <> mustHaveOutput (GYTxOut sendTo (valueFromLovelace 10000000) Nothing Nothing)
                <> mustBeSignedBy pkh

    balance <- runGYTxGameMonadIO nid providers $ asUser w1 $ do
        txBody <- buildTxBody skeleton
        let tx = signGYTxBody txBody [skey]
        getTxBalance pkh tx

    encodeFile outFile balance

balancingPlonkup :: Transaction -> IO ()
balancingPlonkup (Transaction path pathCfg txIn1 txIn2 sig changeAddr outFile txidverifierTxFile utxo1Addr) = do
    coreCfg <- coreConfigIO pathCfg
    (Right (APaymentSigningWitness sks)) <- readWitnessSigningData sig
    (Just txidTx) <- decodeFileStrict txidverifierTxFile

    let testData = path </> "test-data"
    IdentityCircuitContract x ps <- fromJust . decode <$> BL.readFile (testData </> "plonkupVerifierTx-contract-data.json")

    let (setup, _, _) = identityCircuitVerificationBytes x ps

    let nid         = cfgNetworkId coreCfg
        skey        = signingKeyFromApi sks
        changeAddr'  = addressFromApi changeAddr
        utxo1Addr'   = addressFromApi utxo1Addr
        verifierTxScript = validatorFromPlutus @PlutusV3 $ plonkupVerifierTxCompiled setup
        verifyTxAddr     = addressFromValidator nid verifierTxScript
        verifyTx         = txOutRefFromApi txIn2
        parkSpotAddr = addressFromValidator nid $ validatorFromPlutus @PlutusV3 $ parkingSpotCompiled 54
        txIn1'        = GYTxIn (txOutRefFromApi txIn1) GYTxInWitnessKey
        txIn2'        = GYTxIn (txOutRefFromApi txIn2) GYTxInWitnessKey

    let range   = Interval (LowerBound (NegInf:: Extended POSIXTime) True) (UpperBound PosInf True)
        rangeBD = toBuiltinData range

    let addr1 = addressToPlutus changeAddr'
    let val1 = Lovelace 10000000
    let out1   = TxOut addr1 (lovelaceValue val1) NoOutputDatum Nothing
        txOuts = [out1]

    withCfgProviders coreCfg "main" $ \providers -> do
        txin1 <- fmap utxoToTxInInfo . utxosToList <$> gyQueryUtxosAtAddress providers utxo1Addr' Nothing
        txin2 <- fmap utxoToTxInInfo . utxosToList <$> gyQueryUtxosAtAddress providers verifyTxAddr Nothing
        txRefs <- fmap utxoToTxInInfo . utxosToList <$> gyQueryUtxosAtAddress providers parkSpotAddr Nothing
        let txIns = txin1 ++ txin2

        putStr $ "\nInputs:\n" ++ show txIns ++ "\n\n"
        putStr $ "Reference inputs:\n" ++ show txRefs ++ "\n\n"
        putStr $ "Outputs:\n" ++ show txOuts ++ "\n\n"

        let txInsSorted = sortBy (\u v -> outRefCompare (txInInfoOutRef u) (txInInfoOutRef v)) txIns
        let txInsBD  = toBuiltinData txInsSorted
            txRefsBD = toBuiltinData txRefs
            txOutsBD = toBuiltinData txOuts
            txDataBD = mkTuple4 txInsBD txRefsBD txOutsBD rangeBD

        let input = toInput . blake2b_224 . BI.serialiseData $ txDataBD
        putStr $ "Verifier's input: " ++ show input ++ "\n\n"

        putStr "Generating proof...\n\n"
        let (_, _, proof) = stateCheckVerificationBytes x ps input
        let datum = Datum $ toBuiltin $ toData ()
            redeemer = redeemerFromPlutus' $ toBuiltinData proof

        calculateBalancing nid providers skey changeAddr' txIn1' txIn2' changeAddr' verifyTx verifierTxScript txidTx datum redeemer outFile


----- HELPER FUNCTIONS -----

fmapTxId :: V2.TxId -> V3.TxId
fmapTxId (V2.TxId a) = V3.TxId a

fmapTxOutRef :: V2.TxOutRef -> V3.TxOutRef
fmapTxOutRef (V2.TxOutRef a b) = V3.TxOutRef (fmapTxId a) b

utxoToTxInInfo :: GYUTxO -> Plutus.TxInInfo
utxoToTxInInfo GYUTxO {..} =
  Plutus.TxInInfo
    { txInInfoOutRef = fmapTxOutRef $ txOutRefToPlutus utxoRef
    , txInInfoResolved = Plutus.TxOut
      { Plutus.txOutAddress = addressToPlutus utxoAddress
      , Plutus.txOutValue = valueToPlutus utxoValue
      , Plutus.txOutDatum = outDatumToPlutus utxoOutDatum
      , Plutus.txOutReferenceScript = scriptHashToPlutus . hashAnyScript <$> utxoRefScript
      }
    }

mkTuple4 :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData
mkTuple4 a b c d =
  BI.mkList $
    BI.mkCons a $
      BI.mkCons b $
        BI.mkCons c $
          BI.mkCons d $
            BI.mkNilData BI.unitval
