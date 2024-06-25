{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.SymbolicVerifier where

import           GHC.ByteOrder                            (ByteOrder (..))
import           PlutusLedgerApi.V3                       (ScriptContext (..), TxInfo (..))
import           PlutusLedgerApi.V3.Contexts              (TxInInfo (..))
import           PlutusTx                                 (toBuiltinData)
import           PlutusTx.Builtins                        (byteStringToInteger, serialiseData, blake2b_224)
import           PlutusTx.Prelude                         (Bool (..), ($), (.), (<$>))

import           ZkFold.Base.Algebra.Basic.Class          (AdditiveGroup (..))
import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (InputBytes (..), ProofBytes, SetupBytes, toF)

-- | The Plutus script for verifying a ZkFold Symbolic smart contract on the current transaction.
--
-- ZkFold Symbolic smart contracts have access to inputs, reference inputs, outputs and the transaction validity range.
-- Other TxInfo fields can either be passed to the Symbolic contract as private inputs or are not particularly useful inside a ZK contract.
{-# INLINABLE symbolicVerifier #-}
symbolicVerifier :: SetupBytes -> ProofBytes -> ScriptContext -> Bool
symbolicVerifier contract proof ctx =
    -- Verifying the Plonk `proof` for the `contract` on the transaction data encoded as `input`
    verify @PlonkPlutus contract input proof
    where
        info  = scriptContextTxInfo ctx

        -- Extracting transaction data
        ins   = txInInfoOutRef <$> txInfoInputs info
        refs  = txInInfoOutRef <$> txInfoReferenceInputs info
        outs  = txInfoOutputs info
        range = txInfoValidRange info

        -- Computing public input from the transaction data
        input = InputBytes . negate . toF . byteStringToInteger BigEndian . blake2b_224 . serialiseData . toBuiltinData $ (ins, refs, outs, range)
