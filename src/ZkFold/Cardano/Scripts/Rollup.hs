{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.Scripts.Rollup where

import           GHC.ByteOrder                            (ByteOrder(..))
import           PlutusLedgerApi.V3
import           PlutusLedgerApi.V3.Contexts              (findOwnInput)
import           PlutusTx.Prelude                         hiding ((*), (+))

import           ZkFold.Base.Algebra.Basic.Class          ((*), (+))
import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (F(..))
import           ZkFold.Cardano.Plonk.OnChain.Data        (ProofBytes, SetupBytes, InputBytes (..))
import           ZkFold.Cardano.Plonk.OnChain.Utils       (dataToBlake)

-- | Plutus script for verifying a ZkFold Rollup state transition.
{-# INLINABLE rollup #-}
rollup :: SetupBytes -> (ProofBytes, Address, Value, F, [F]) -> ScriptContext -> Bool
rollup ledgerRules (proof, addr, val, state, update) ctx =
        -- Verify the transition from the current state to the next state
        verify @PlonkPlutus ledgerRules input proof
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
        x      = F . byteStringToInteger BigEndian $ dataToBlake (state, update)
        state' = foldl (\acc u -> x * acc + u) state update

        -- Computing public input from the transaction data
        input  = InputBytes state'
