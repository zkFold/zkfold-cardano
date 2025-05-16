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
import           PlutusTx                              (CompiledCode, UnsafeFromData (..), compile, liftCodeDef,
                                                        makeIsDataIndexed, makeLift, unsafeApplyCode)
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
      { maybeSwitchAddress :: Maybe Address
      , maybeNextAddress   :: Maybe Address
      , nextGroupElement   :: BuiltinByteString
      , switchGroupElement :: BuiltinByteString
      , utxoValue          :: Value
      }
  deriving stock (Show, Generic)

makeIsDataIndexed ''UtxoAccumulatorParameters [('UtxoAccumulatorParameters,0)]
makeLift ''UtxoAccumulatorParameters

data UtxoAccumulatorRedeemer =
      AddUtxo Integer
    | RemoveUtxo Address ProofBytes
    | Switch
  deriving stock (Show, Generic)

makeIsDataIndexed ''UtxoAccumulatorRedeemer [('AddUtxo,0),('RemoveUtxo,1),('Switch,2)]
makeLift ''UtxoAccumulatorRedeemer

{-# INLINABLE utxoAccumulator #-}
utxoAccumulator :: UtxoAccumulatorParameters -> UtxoAccumulatorRedeemer -> ScriptContext -> Bool
utxoAccumulator UtxoAccumulatorParameters {..} (AddUtxo h) ctx =
  let
    Just (TxInInfo _ (TxOut _ v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

    Just nextAddress = maybeNextAddress

    v' = v + utxoValue

    setup  = unsafeFromBuiltinData d :: SetupBytes
    setup' = updateSetupBytes setup h nextGroupElement
    d' = toBuiltinData setup'

    outputAcc = head $ txInfoOutputs $ scriptContextTxInfo ctx
  in
    outputAcc == TxOut nextAddress v' (OutputDatum (Datum d')) Nothing
utxoAccumulator UtxoAccumulatorParameters {..} (RemoveUtxo addr proof) ctx =
  let
    Just (TxInInfo _ (TxOut _ v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

    Just nextAddress = maybeNextAddress

    v' = v - utxoValue

    a = byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr

    setup  = unsafeFromBuiltinData d :: SetupBytes
    setup' = updateSetupBytes setup a nextGroupElement
    d' = toBuiltinData setup'

    outputAcc  = head $ txInfoOutputs $ scriptContextTxInfo ctx
    outputUser = head $ tail $ txInfoOutputs $ scriptContextTxInfo ctx
  in
       outputAcc  == TxOut nextAddress v' (OutputDatum (Datum d')) Nothing
    && outputUser == TxOut addr utxoValue NoOutputDatum Nothing
    && verify @PlonkupPlutus setup [] proof
utxoAccumulator UtxoAccumulatorParameters {..} Switch ctx =
  let
    Just (TxInInfo _ (TxOut _ v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

    Just switchAddress = maybeSwitchAddress

    setup  = unsafeFromBuiltinData d :: SetupBytes
    setup' = updateSetupBytes setup 1 switchGroupElement
    d' = toBuiltinData setup'

    outputAcc = head $ txInfoOutputs $ scriptContextTxInfo ctx
  in
    outputAcc == TxOut switchAddress v (OutputDatum (Datum d')) Nothing

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
