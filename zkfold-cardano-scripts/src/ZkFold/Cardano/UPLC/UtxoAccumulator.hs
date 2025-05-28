{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.UPLC.UtxoAccumulator where

import           GHC.Generics                          (Generic)
import           PlutusLedgerApi.V3                    (Address, Datum (..), OutputDatum (NoOutputDatum, OutputDatum),
                                                        Redeemer (..), ScriptContext (..), ToData (..), TxInInfo (..),
                                                        TxInfo (..), TxOut (..), Value)
import           PlutusLedgerApi.V3.Contexts           (findOwnInput)
import           PlutusTx                              (CompiledCode, UnsafeFromData (..), compile, makeIsDataIndexed, makeLift, unsafeApplyCode, liftCodeDef)
import           PlutusTx.Builtins                     (ByteOrder (..), serialiseData)
import           PlutusTx.Prelude                      (AdditiveGroup (..), Bool, BuiltinByteString, BuiltinData,
                                                        BuiltinUnit, Eq (..), Integer, Maybe (..), blake2b_224,
                                                        byteStringToInteger, check, head, tail, ($), (&&), (+), (.))
import           Prelude                               (Show)

import           ZkFold.Cardano.OnChain.Plonkup        (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data   (ProofBytes, SetupBytes)
import           ZkFold.Cardano.OnChain.Plonkup.Update (updateSetupBytes)
import           ZkFold.Protocol.NonInteractiveProof   (NonInteractiveProof (..))

data UtxoAccumulatorParameters =
    UtxoAccumulatorParameters
      { switchGroupElement  :: BuiltinByteString
      , switchDatumHash     :: BuiltinByteString
      , accumulationValue   :: Value
      }
  deriving stock (Show, Generic)

makeIsDataIndexed ''UtxoAccumulatorParameters [('UtxoAccumulatorParameters,0)]
makeLift ''UtxoAccumulatorParameters

data UtxoAccumulatorDatum =
    UtxoAccumulatorDatum
      { currentGroupElement :: BuiltinByteString
      , maybeNextDatumHash  :: Maybe BuiltinByteString
      , canSwitch           :: Bool
      }
  deriving stock (Show, Generic)

makeIsDataIndexed ''UtxoAccumulatorDatum [('UtxoAccumulatorDatum,0)]
makeLift ''UtxoAccumulatorDatum

data UtxoAccumulatorRedeemer =
      AddUtxo Integer UtxoAccumulatorDatum
    | RemoveUtxo Address ProofBytes UtxoAccumulatorDatum
    | Switch UtxoAccumulatorDatum
  deriving stock (Show, Generic)

makeIsDataIndexed ''UtxoAccumulatorRedeemer [('AddUtxo,0),('RemoveUtxo,1),('Switch,2)]
makeLift ''UtxoAccumulatorRedeemer

{-# INLINABLE utxoAccumulator #-}
utxoAccumulator :: UtxoAccumulatorParameters -> UtxoAccumulatorRedeemer -> ScriptContext -> Bool
utxoAccumulator UtxoAccumulatorParameters {..} (AddUtxo h dat') ctx =
  let
    Just (TxInInfo _ (TxOut ownAddr v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

    (UtxoAccumulatorDatum {..}, setup)  = unsafeFromBuiltinData d :: (UtxoAccumulatorDatum, SetupBytes)
    setup' = updateSetupBytes setup h currentGroupElement
    d' = toBuiltinData (dat', setup')

    v' = v + accumulationValue

    outputAcc = head $ txInfoOutputs $ scriptContextTxInfo ctx
  in
    outputAcc == TxOut ownAddr v' (OutputDatum (Datum d')) Nothing
    && maybeNextDatumHash == Just (blake2b_224 $ serialiseData $ toBuiltinData dat')
utxoAccumulator UtxoAccumulatorParameters {..} (RemoveUtxo addr proof dat') ctx =
  let
    Just (TxInInfo _ (TxOut ownAddr v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

    a = byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr

    (UtxoAccumulatorDatum {..}, setup)  = unsafeFromBuiltinData d :: (UtxoAccumulatorDatum, SetupBytes)
    setup' = updateSetupBytes setup a currentGroupElement
    d' = toBuiltinData (dat', setup')

    v' = v - accumulationValue

    outputAcc  = head $ txInfoOutputs $ scriptContextTxInfo ctx
    outputUser = head $ tail $ txInfoOutputs $ scriptContextTxInfo ctx
  in
       outputAcc  == TxOut ownAddr v' (OutputDatum (Datum d')) Nothing
    && outputUser == TxOut addr accumulationValue NoOutputDatum Nothing
    && verify @PlonkupPlutus setup [] proof
    && maybeNextDatumHash == Just (blake2b_224 $ serialiseData $ toBuiltinData dat')
utxoAccumulator UtxoAccumulatorParameters {..} (Switch dat') ctx =
  let
    Just (TxInInfo _ (TxOut ownAddr v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

    (UtxoAccumulatorDatum {..}, setup) = unsafeFromBuiltinData d :: (UtxoAccumulatorDatum, SetupBytes)
    setup' = updateSetupBytes setup 1 switchGroupElement
    d' = toBuiltinData (dat', setup')

    outputAcc = head $ txInfoOutputs $ scriptContextTxInfo ctx
  in
    outputAcc == TxOut ownAddr v (OutputDatum (Datum d')) Nothing
    && switchDatumHash == blake2b_224 (serialiseData $ toBuiltinData dat')
    && canSwitch

{-# INLINABLE untypedUtxoAccumulator #-}
untypedUtxoAccumulator :: UtxoAccumulatorParameters -> BuiltinData -> BuiltinUnit
untypedUtxoAccumulator par ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ utxoAccumulator par redeemer ctx

utxoAccumulatorCompiled :: UtxoAccumulatorParameters -> CompiledCode (BuiltinData -> BuiltinUnit)
utxoAccumulatorCompiled par =
    $$(compile [|| untypedUtxoAccumulator ||])
      `unsafeApplyCode` liftCodeDef par
