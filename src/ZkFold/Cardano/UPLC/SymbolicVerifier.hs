module ZkFold.Cardano.UPLC.SymbolicVerifier where

import           PlutusLedgerApi.V3                       (ScriptContext (..), TxInfo (..), getRedeemer)
import           PlutusTx                                 (unsafeFromBuiltinData)
import           PlutusTx.Prelude                         (Bool (..), BuiltinData, BuiltinUnit, check, ($), (.))

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain.BLS12_381         (toInput)
import           ZkFold.Cardano.OnChain.Plonk             (PlonkPlutus)
import           ZkFold.Cardano.OnChain.Plonk.Data        (ProofBytes, SetupBytes)
import           ZkFold.Cardano.OnChain.Utils             (dataToBlake)

-- | Plutus script for verifying a ZkFold Symbolic smart contract on the current transaction.
--
-- ZkFold Symbolic smart contracts have access to inputs, reference inputs, outputs and the transaction validity range.
-- Other TxInfo fields can either be passed to the Symbolic contract as private inputs or are not particularly useful inside a ZK contract.
{-# INLINABLE symbolicVerifier #-}
symbolicVerifier :: SetupBytes -> ProofBytes -> ScriptContext -> Bool
symbolicVerifier contract proof ctx =
    -- Verifying the Plonk `proof` for the `contract` on the transaction data encoded as `input`
    verify @PlonkPlutus @HaskellCore contract input proof
    where
        info  = scriptContextTxInfo ctx

        -- Extracting transaction data
        ins   = txInfoInputs info
        refs  = txInfoReferenceInputs info
        outs  = txInfoOutputs info
        range = txInfoValidRange info

        -- Computing public input from the transaction data
        input = toInput . dataToBlake $ (ins, refs, outs, range)

{-# INLINABLE untypedSymbolicVerifier #-}
untypedSymbolicVerifier :: SetupBytes -> BuiltinData -> BuiltinUnit
untypedSymbolicVerifier contract' ctx' =
    let
      ctx           = unsafeFromBuiltinData ctx'
      redeemerProof = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
    in
      check $ symbolicVerifier contract' redeemerProof ctx
