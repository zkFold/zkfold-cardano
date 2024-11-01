{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.UPLC.Rollup where

import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V3
import           PlutusLedgerApi.V3.Contexts              (findOwnInput)
import           PlutusTx                                 (makeIsDataIndexed)
import           PlutusTx.Prelude
import           Prelude                                  (Show)

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain.BLS12_381         (F (..), toInput)
import           ZkFold.Cardano.OnChain.Plonk             (PlonkPlutus)
import           ZkFold.Cardano.OnChain.Plonk.Data        (ProofBytes, SetupBytes)
import           ZkFold.Cardano.OnChain.Utils             (dataToBlake)

data RollupRedeemer = RollupRedeemer
  { rrProof   :: ProofBytes
  , rrAddress :: Address
  , rrValue   :: Value
  , rrState   :: F
  , rrUpdate  :: [F]
  } deriving stock (Show, Generic)

makeIsDataIndexed ''RollupRedeemer [('RollupRedeemer,0)]

-- | Plutus script for verifying a ZkFold Rollup state transition.
{-# INLINABLE rollup #-}
rollup :: SetupBytes -> RollupRedeemer -> ScriptContext -> Bool
rollup ledgerRules RollupRedeemer{..} ctx =
        -- Verify the transition from the current state to the next state
        verify @PlonkPlutus @HaskellCore ledgerRules nextState rrProof
        -- Check the current rollup output
        && out  == TxOut rrAddress rrValue (OutputDatum $ Datum $ toBuiltinData rrState) Nothing
        -- Check the next rollup output
        && out' == TxOut rrAddress rrValue (OutputDatum $ Datum $ toBuiltinData nextState) Nothing
    where
        -- Get the current rollup output
        Just j = findOwnInput ctx
        out    = txInInfoResolved j

        -- Get the next rollup output
        out'   = head $ txInfoOutputs $ scriptContextTxInfo ctx

        -- Compute the next state
        nextState = toInput $ dataToBlake (rrState, rrUpdate)

rollupMini :: SetupBytes -> RollupRedeemer -> Bool
rollupMini ledgerRules RollupRedeemer{..} =
        -- Verify the transition from the current state to the next state
        verify @PlonkPlutus @HaskellCore ledgerRules nextState rrProof
    where
        -- Compute the next state
        nextState = toInput $ dataToBlake (rrState, rrUpdate)

{-# INLINABLE rollup' #-}
rollup' :: SetupBytes -> RollupRedeemer -> ScriptContext -> Bool
rollup' ledgerRules RollupRedeemer{..} ctx =
        -- Tautology: Verify the transition from the current state to the next state, then always True
        (verify @PlonkPlutus @HaskellCore ledgerRules nextState rrProof || True)
        -- Check the current rollup output
        && out  == TxOut rrAddress rrValue (OutputDatum $ Datum $ toBuiltinData rrState) Nothing
        -- Check the next rollup output
        && out' == TxOut rrAddress rrValue (OutputDatum $ Datum $ toBuiltinData nextState) Nothing
    where
        -- Get the current rollup output
        Just j = findOwnInput ctx
        out    = txInInfoResolved j

        -- Get the next rollup output
        out'   = head $ txInfoOutputs $ scriptContextTxInfo ctx

        -- Compute the next state
        nextState = toInput $ dataToBlake (rrState, rrUpdate)

{-# INLINABLE parkingSpot #-}
parkingSpot :: Integer -> ScriptContext -> Bool
parkingSpot _ _ = True


{-# INLINABLE untypedRollup #-}
untypedRollup :: SetupBytes -> BuiltinData -> BuiltinUnit
untypedRollup computation ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ rollup computation redeemer ctx

{-# INLINABLE untypedRollup' #-}
untypedRollup' :: SetupBytes -> BuiltinData -> BuiltinUnit
untypedRollup' computation ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ rollup' computation redeemer ctx

{-# INLINABLE untypedParkingSpot #-}
untypedParkingSpot :: Integer -> BuiltinData -> BuiltinUnit
untypedParkingSpot tag ctx' =
  let
    ctx = unsafeFromBuiltinData ctx'
  in
    check $ parkingSpot tag ctx
