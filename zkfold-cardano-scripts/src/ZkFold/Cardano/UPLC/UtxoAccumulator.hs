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

type UtxoAccumulatorParameters = Value

data UtxoAccumulatorDatum =
    UtxoAccumulatorDatum
      { maybeCurrentGroupElement :: Maybe BuiltinByteString
      , nextDatumHash            :: BuiltinByteString
      }
  deriving stock (Show, Generic)

makeIsDataIndexed ''UtxoAccumulatorDatum [('UtxoAccumulatorDatum,0)]
makeLift ''UtxoAccumulatorDatum

data UtxoAccumulatorRedeemer =
      AddUtxo Integer UtxoAccumulatorDatum
    | RemoveUtxo Address ProofBytes UtxoAccumulatorDatum
  deriving stock (Show, Generic)

makeIsDataIndexed ''UtxoAccumulatorRedeemer [('AddUtxo,0),('RemoveUtxo,1)]
makeLift ''UtxoAccumulatorRedeemer

{-# INLINABLE utxoAccumulator #-}
utxoAccumulator :: UtxoAccumulatorParameters -> UtxoAccumulatorRedeemer -> ScriptContext -> Bool
utxoAccumulator accumulationValue (AddUtxo h dat') ctx =
  let
    Just (TxInInfo _ (TxOut ownAddr v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

    (UtxoAccumulatorDatum {..}, datumRemove, setup)  = unsafeFromBuiltinData d :: (UtxoAccumulatorDatum, UtxoAccumulatorDatum, SetupBytes)
    setup' = updateSetupBytes setup h $ fromMaybe (error ()) maybeCurrentGroupElement
    d' = toBuiltinData (dat', datumRemove, setup')

    v' = v + accumulationValue

    outputAcc = head $ txInfoOutputs $ scriptContextTxInfo ctx
  in
    outputAcc == TxOut ownAddr v' (OutputDatum (Datum d')) Nothing
    && nextDatumHash == blake2b_224 (serialiseData $ toBuiltinData dat')
utxoAccumulator accumulationValue (RemoveUtxo addr proof dat') ctx =
  let
    Just (TxInInfo _ (TxOut ownAddr v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

    a = byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr

    (datumAdd, UtxoAccumulatorDatum {..}, setup)  = unsafeFromBuiltinData d :: (UtxoAccumulatorDatum, UtxoAccumulatorDatum, SetupBytes)
    setup' = updateSetupBytes setup a $ fromMaybe (error ()) maybeCurrentGroupElement
    d' = toBuiltinData (datumAdd, dat', setup')

    v' = v - accumulationValue

    outputAcc  = head $ txInfoOutputs $ scriptContextTxInfo ctx
    outputUser = head $ tail $ txInfoOutputs $ scriptContextTxInfo ctx
  in
       outputAcc  == TxOut ownAddr v' (OutputDatum (Datum d')) Nothing
    && outputUser == TxOut addr accumulationValue NoOutputDatum Nothing
    && verify @PlonkupPlutus setup [] proof
    && nextDatumHash == blake2b_224 (serialiseData $ toBuiltinData dat')

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
