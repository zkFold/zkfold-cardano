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
import           PlutusTx                              (CompiledCode, UnsafeFromData (..), compile, makeIsDataIndexed, makeLift)
import           PlutusTx.Builtins                     (ByteOrder (..), serialiseData, error)
import           PlutusTx.Prelude                      (AdditiveGroup (..), Bool (..), BuiltinByteString, BuiltinData,
                                                        BuiltinUnit, Eq (..), Integer, Maybe (..), blake2b_224,
                                                        byteStringToInteger, check, head, tail, ($), (&&), (+), (.))
import           Prelude                               (Show)

import           ZkFold.Cardano.OnChain.Plonkup        (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data   (ProofBytes, SetupBytes)
import           ZkFold.Cardano.OnChain.Plonkup.Update (updateSetupBytes)
import           ZkFold.Protocol.NonInteractiveProof   (NonInteractiveProof (..))

data UtxoAccumulatorParameters =
    UtxoAccumulatorParameters
      { maybeNextParHash    :: Maybe BuiltinByteString
      , maybeSwitchParHash  :: Maybe BuiltinByteString
      , currentGroupElement :: BuiltinByteString
      , switchGroupElement  :: BuiltinByteString
      , accumulationValue   :: Value
      }
  deriving stock (Show, Generic)

makeIsDataIndexed ''UtxoAccumulatorParameters [('UtxoAccumulatorParameters,0)]
makeLift ''UtxoAccumulatorParameters

data UtxoAccumulatorRedeemer =
      AddUtxo Integer UtxoAccumulatorParameters
    | RemoveUtxo Address ProofBytes UtxoAccumulatorParameters
    | Switch UtxoAccumulatorParameters
  deriving stock (Show, Generic)

makeIsDataIndexed ''UtxoAccumulatorRedeemer [('AddUtxo,0),('RemoveUtxo,1),('Switch,2)]
makeLift ''UtxoAccumulatorRedeemer

{-# INLINABLE utxoAccumulator #-}
utxoAccumulator :: UtxoAccumulatorRedeemer -> ScriptContext -> Bool
utxoAccumulator redeemer ctx =
  let
    Just (TxInInfo _ (TxOut ownAddr v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx
    (UtxoAccumulatorParameters {..}, setup)  = unsafeFromBuiltinData d :: (UtxoAccumulatorParameters, SetupBytes)

    (x, g, par', v', hash) = case redeemer of
      AddUtxo h p'         -> (h, currentGroupElement, p', v + accumulationValue, maybeNextParHash)
      RemoveUtxo addr proof p' ->
        let outputUser = head $ tail $ txInfoOutputs $ scriptContextTxInfo ctx
        in if verify @PlonkupPlutus setup [] proof && outputUser == TxOut addr accumulationValue NoOutputDatum Nothing
          then (byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr, currentGroupElement, p', v - accumulationValue, maybeNextParHash)
          else error ()
      Switch p'            -> (1, switchGroupElement, p', v, maybeSwitchParHash)

    setup' = updateSetupBytes setup x g
    d' = toBuiltinData (par', setup')

    outputAcc = head $ txInfoOutputs $ scriptContextTxInfo ctx
  in
    outputAcc == TxOut ownAddr v' (OutputDatum (Datum d')) Nothing
    && hash == Just (blake2b_224 $ serialiseData $ toBuiltinData par')
-- utxoAccumulator (AddUtxo h par') ctx =
--   let
--     Just (TxInInfo _ (TxOut ownAddr v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

--     (UtxoAccumulatorParameters {..}, setup)  = unsafeFromBuiltinData d :: (UtxoAccumulatorParameters, SetupBytes)
--     setup' = updateSetupBytes setup h currentGroupElement
--     d' = toBuiltinData (par', setup')

--     v' = v + accumulationValue

--     outputAcc = head $ txInfoOutputs $ scriptContextTxInfo ctx
--   in
--     outputAcc == TxOut ownAddr v' (OutputDatum (Datum d')) Nothing
--     && maybeNextParHash == Just (blake2b_224 $ serialiseData $ toBuiltinData par')
-- utxoAccumulator (RemoveUtxo addr proof par') ctx =
--   let
--     Just (TxInInfo _ (TxOut ownAddr v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

--     a = byteStringToInteger BigEndian $ blake2b_224 $ serialiseData $ toBuiltinData addr

--     (UtxoAccumulatorParameters {..}, setup)  = unsafeFromBuiltinData d :: (UtxoAccumulatorParameters, SetupBytes)
--     setup' = updateSetupBytes setup a currentGroupElement
--     d' = toBuiltinData (par', setup')

--     v' = v - accumulationValue

--     outputAcc  = head $ txInfoOutputs $ scriptContextTxInfo ctx
--     outputUser = head $ tail $ txInfoOutputs $ scriptContextTxInfo ctx
--   in
--        outputAcc  == TxOut ownAddr v' (OutputDatum (Datum d')) Nothing
--     && outputUser == TxOut addr accumulationValue NoOutputDatum Nothing
--     && verify @PlonkupPlutus setup [] proof
--     && maybeNextParHash == Just (blake2b_224 $ serialiseData $ toBuiltinData par')
-- utxoAccumulator (Switch par') ctx =
--   let
--     Just (TxInInfo _ (TxOut ownAddr v (OutputDatum (Datum d)) Nothing))  = findOwnInput ctx

--     (UtxoAccumulatorParameters {..}, setup) = unsafeFromBuiltinData d :: (UtxoAccumulatorParameters, SetupBytes)
--     setup' = updateSetupBytes setup 1 switchGroupElement
--     d' = toBuiltinData (par', setup')

--     outputAcc = head $ txInfoOutputs $ scriptContextTxInfo ctx
--   in
--     outputAcc == TxOut ownAddr v (OutputDatum (Datum d')) Nothing
--     && maybeSwitchParHash == Just (blake2b_224 $ serialiseData $ toBuiltinData par')

{-# INLINABLE untypedUtxoAccumulator #-}
untypedUtxoAccumulator :: BuiltinData -> BuiltinUnit
untypedUtxoAccumulator ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ utxoAccumulator redeemer ctx

utxoAccumulatorCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
utxoAccumulatorCompiled =
    $$(compile [|| untypedUtxoAccumulator ||])
