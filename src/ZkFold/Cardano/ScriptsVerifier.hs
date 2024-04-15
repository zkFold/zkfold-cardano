{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.ScriptsVerifier (symbolicVerifier, plonkVerifier) where

import           PlutusLedgerApi.V1.Value                 (Value (..))
import           PlutusLedgerApi.V3                       (ScriptContext (..), TokenName (..), TxInfo (..))
import           PlutusLedgerApi.V3.Contexts              (ownCurrencySymbol)
import           PlutusTx                                 (toBuiltinData)
import qualified PlutusTx.AssocMap                        as AssocMap
import           PlutusTx.Builtins                        (blake2b_224, serialiseData)
import           PlutusTx.Prelude                         (Bool (..), Eq (..), Maybe (..), Ord (..), ($), (&&), (.), (||))
import qualified PlutusTx.Prelude                         as Plutus

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..), ToTranscript (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (InputBytes (..))

-- TODO: split the setup data into the fixed and varying parts
-- | The Plutus script for verifying a ZkFold Symbolic smart contract.
{-# INLINABLE symbolicVerifier #-}
symbolicVerifier :: Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> Bool
symbolicVerifier contract input proof ctx = condition1 && condition2
    where
        info  = scriptContextTxInfo ctx
        ins   = txInfoInputs info
        outs  = txInfoOutputs info
        refs  = txInfoReferenceInputs info
        range = txInfoValidRange info

        h     = blake2b_224 . serialiseData . toBuiltinData $ (ins, refs, outs, range)

        -- Verifying that the public input in the ZKP protocol corresponds to the hash of the transaction data.
        --
        -- ZkFold Symbolic smart contracts will have access to inputs, reference inputs, outputs and the transaction validity range.
        -- Other TxInfo fields can either be passed to the Symbolic contract as private inputs or are not particularly useful inside a contract.
        condition1 = serialiseData (toBuiltinData input) == h

        -- Verifying the validity of the ZkFold Symbolic smart contract on the current transaction.
        -- The smart contract is encoded into the `Setup PlonkPlutus` data structure.
        condition2 = verify @PlonkPlutus contract input proof

-- | The Plutus script (minting policy) for verifying a Plonk proof.
{-# INLINABLE plonkVerifier #-}
plonkVerifier :: Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> Bool
plonkVerifier computation input proof ctx = condition0 && (condition1 || condition2)
    where
        info               = scriptContextTxInfo ctx
        Just m             = AssocMap.lookup (ownCurrencySymbol ctx) (getValue $ txInfoMint info)
        [(TokenName t, n)] = AssocMap.toList m

        -- With this minting policy, we can mint tokens if the Plonk proof is valid for the input provided in the redeemer.
        -- The tokens serve as proof that the network has verified the computation.
        -- We can also burn already minted tokens.

        -- Verifying that the token name equals to the bytestring representation of the public input in the ZKP protocol
        condition0 = t == toTranscript (Plutus.head $ pubInput input)

        -- Burning already minted tokens
        condition1 = n < 0

        -- Verifying the Plonk proof
        condition2 = verify @PlonkPlutus computation input proof
