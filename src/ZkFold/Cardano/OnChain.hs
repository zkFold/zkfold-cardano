{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell  #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.OnChain where

import           GHC.ByteOrder                            (ByteOrder(..))
import           PlutusLedgerApi.V1.Value                 (Value (..))
import           PlutusLedgerApi.V3                       (ScriptContext (..), TxInfo (..), TxInInfo (..), TokenName (..))
import           PlutusLedgerApi.V3.Contexts              (ownCurrencySymbol)
import           PlutusTx                                 (CompiledCode, toBuiltinData)
import           PlutusTx.Builtins                        hiding (head)
import           PlutusTx.Prelude                         (Eq (..), Bool (..), Maybe (..), Ord (..), ($), (||), (&&))
import qualified PlutusTx.Prelude                         as Plutus
import qualified PlutusTx.AssocMap                        as AssocMap
import           PlutusTx.TH                              (compile)
import           Prelude                                  ((.))

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof(..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.Internal            (toF)

-- TODO: split the setup data into the fixed and varying parts
-- | The Plutus script for verifying a ZkFold Symbolic smart contract.
{-# INLINABLE symbolicVerifier #-}
symbolicVerifier :: (Setup PlonkPlutus, Input PlonkPlutus, Proof PlonkPlutus) -> ScriptContext -> Bool
symbolicVerifier (contract, input, proof) ctx = condition1 && condition2
    where
        info  = scriptContextTxInfo ctx
        ins   = Plutus.map txInInfoOutRef (txInfoInputs info)
        outs  = txInfoOutputs info
        refs  = Plutus.map txInInfoOutRef (txInfoReferenceInputs info)
        range = txInfoValidRange info

        h     = blake2b_224 . serialiseData . toBuiltinData $ (ins, refs, outs, range)

        -- Verifying that the public input in the ZKP protocol corresponds to the hash of the transaction data.
        --
        -- ZkFold Symbolic smart contracts will have access to inputs, reference inputs, outputs and the transaction validity range.
        -- Other TxInfo fields can either be passed to the Symbolic contract as private inputs or are not particularly useful inside a contract.
        -- For inputs and reference inputs, we only need the references as we can supply the past transaction data as private inputs.
        condition1 = serialiseData (toBuiltinData input) == h

        -- Verifying the validity of the ZkFold Symbolic smart contract on the current transaction.
        -- The smart contract is encoded into the `Setup PlonkPlutus` data structure.
        condition2 = verify @PlonkPlutus contract input proof

compiledSymbolicVerifier :: CompiledCode ((Setup PlonkPlutus, Input PlonkPlutus, Proof PlonkPlutus) -> ScriptContext -> Bool)
compiledSymbolicVerifier = $$(compile [|| symbolicVerifier ||])

-- | The Plutus script (minting policy) for verifying a Plonk proof.
{-# INLINABLE plonkVerifier #-}
plonkVerifier :: (Setup PlonkPlutus, Input PlonkPlutus, Proof PlonkPlutus) -> ScriptContext -> Bool
plonkVerifier (computation, input, proof) ctx = condition0 && (condition1 || condition2)
    where
        info               = scriptContextTxInfo ctx
        Just m             = AssocMap.lookup (ownCurrencySymbol ctx) (getValue $ txInfoMint info)
        [(TokenName t, n)] = AssocMap.toList m

        -- With this minting policy, we can mint tokens if the Plonk proof is valid for the input provided in the redeemer.
        -- The tokens serve as proof that the network has verified the computation.
        -- We can also burn already minted tokens.

        -- Verifying that the token name equals to the bytestring representation of the public input in the ZKP protocol
        condition0 = t == integerToByteString BigEndian 0 (toF input)

        -- Burning already minted tokens
        condition1 = n < 0

        -- Verifying the Plonk proof
        condition2 = verify @PlonkPlutus computation input proof

compiledPlonkVerifier :: CompiledCode ((Setup PlonkPlutus, Input PlonkPlutus, Proof PlonkPlutus) -> ScriptContext -> Bool)
compiledPlonkVerifier = $$(compile [|| plonkVerifier ||])