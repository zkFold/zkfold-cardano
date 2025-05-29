{-# LANGUAGE TemplateHaskell #-}

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
import           PlutusTx.Builtins                     (ByteOrder (..), serialiseData, error)
import           PlutusTx.Prelude                      (AdditiveGroup (..), Bool, BuiltinByteString, BuiltinData,
                                                        BuiltinUnit, Eq (..), Integer, Maybe (..), blake2b_224,
                                                        byteStringToInteger, check, head, tail, ($), (&&), (+), (.), fromMaybe)
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

data AddDatum =
    AddDatum
      { maybeAddGroupElement  :: Maybe BuiltinByteString
      , nextAddDatumHash      :: BuiltinByteString
      }
  deriving stock (Show, Generic)

makeIsDataIndexed ''AddDatum [('AddDatum,0)]
makeLift ''AddDatum

data RemoveDatum =
    RemoveDatum
      { maybeRemoveGroupElement :: Maybe BuiltinByteString
      , nextRemoveDatumHash     :: BuiltinByteString
      }
  deriving stock (Show, Generic)

makeIsDataIndexed ''RemoveDatum [('RemoveDatum,0)]
makeLift ''RemoveDatum

data UtxoAccumulatorRedeemer =
      AddUtxo Integer AddDatum
    | RemoveUtxo Address ProofBytes RemoveDatum
  deriving stock (Show, Generic)

makeIsDataIndexed ''UtxoAccumulatorRedeemer [('AddUtxo,0),('RemoveUtxo,1)]
makeLift ''UtxoAccumulatorRedeemer

{-# INLINABLE utxoAccumulator #-}
utxoAccumulator :: UtxoAccumulatorParameters -> UtxoAccumulatorRedeemer -> ScriptContext -> Bool
utxoAccumulator UtxoAccumulatorParameters {..} (AddUtxo h dat') ctx =
  let
    Just (TxInInfo _ (TxOut ownAddr v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

    (AddDatum {..}, datumRemove, setup)  = unsafeFromBuiltinData d :: (AddDatum, RemoveDatum, SetupBytes)
    setup' = updateSetupBytes setup h $ fromMaybe (error ()) maybeAddGroupElement
    d' = toBuiltinData (dat', datumRemove, setup')

    v' = v + accumulationValue

    outputAcc = head $ txInfoOutputs $ scriptContextTxInfo ctx
  in
    outputAcc == TxOut ownAddr v' (OutputDatum (Datum d')) Nothing
    && nextAddDatumHash == blake2b_224 (serialiseData $ toBuiltinData dat')
utxoAccumulator UtxoAccumulatorParameters {..} (RemoveUtxo addr proof dat') ctx =
  let
    Just (TxInInfo _ (TxOut ownAddr v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

    a = byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr

    (datumAdd, RemoveDatum {..}, setup)  = unsafeFromBuiltinData d :: (AddDatum, RemoveDatum, SetupBytes)
    setup' = updateSetupBytes setup a $ fromMaybe (error ()) maybeRemoveGroupElement
    d' = toBuiltinData (datumAdd, dat', setup')

    v' = v - accumulationValue

    outputAcc  = head $ txInfoOutputs $ scriptContextTxInfo ctx
    outputUser = head $ tail $ txInfoOutputs $ scriptContextTxInfo ctx
  in
       outputAcc  == TxOut ownAddr v' (OutputDatum (Datum d')) Nothing
    && outputUser == TxOut addr accumulationValue NoOutputDatum Nothing
    && verify @PlonkupPlutus setup [] proof
    && nextRemoveDatumHash == blake2b_224 (serialiseData $ toBuiltinData dat')

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
