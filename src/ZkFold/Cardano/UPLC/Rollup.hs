{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.UPLC.Rollup where

import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V3
import           PlutusLedgerApi.V3.Contexts              (findOwnInput)
import           PlutusTx                                 (makeIsDataIndexed)
import           PlutusTx.Prelude                         hiding ((*), (+))
import           Prelude                                  (Show)

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..), HaskellCore)
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
rollup ledgerRules (RollupRedeemer proof addr val state update) ctx =
        -- Verify the transition from the current state to the next state
        verify @PlonkPlutus @HaskellCore ledgerRules state' proof
        -- Check the current rollup output
        && out  == TxOut addr val (OutputDatum $ Datum $ toBuiltinData state)  Nothing
        -- Check the next rollup output
        && out' == TxOut addr val (OutputDatum $ Datum $ toBuiltinData state') Nothing
    where
        -- Get the current rollup output
        Just j = findOwnInput ctx
        out    = txInInfoResolved j

        -- Get the next rollup output
        out'   = head $ txInfoOutputs $ scriptContextTxInfo ctx

        -- Compute the next state
        state' = toInput $ dataToBlake (state, update)

{-# INLINABLE untypedRollup #-}
untypedRollup :: SetupBytes -> BuiltinData -> BuiltinUnit
untypedRollup computation ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ rollup computation redeemer ctx
