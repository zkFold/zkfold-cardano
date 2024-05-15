{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ZkFold.Cardano.ScriptsVerifier where

import           GHC.ByteOrder                            (ByteOrder (..))
import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V1.Value                 (Value (..))
import           PlutusLedgerApi.V3                       (ScriptContext (..), TokenName (..), TxInfo (..))
import           PlutusLedgerApi.V3.Contexts              (ownCurrencySymbol)
import           PlutusTx                                 (makeIsDataIndexed, makeLift, toBuiltinData)
import qualified PlutusTx.AssocMap                        as AssocMap
import           PlutusTx.Builtins                        (blake2b_256, byteStringToInteger, serialiseData)
import           PlutusTx.Prelude                         (Bool (..), Eq (..), Maybe (..), Ord (..), takeByteString, ($), (&&), (.), (||))

import           ZkFold.Base.Algebra.Basic.Class          (AdditiveGroup (..))
import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (F (..), InputBytes (..), ProofBytes, SetupBytes)

data ParamsVerifier = ParamsVerifier
  deriving stock (Generic)

makeLift ''ParamsVerifier
makeIsDataIndexed ''ParamsVerifier [('ParamsVerifier, 0)]

data DatumVerifier = DatumVerifier
  deriving stock (Generic)

makeLift ''DatumVerifier
makeIsDataIndexed ''DatumVerifier [('DatumVerifier, 0)]

data RedeemerVerifier = RedeemerVerifier SetupBytes InputBytes ProofBytes
  deriving stock (Generic)

makeLift ''RedeemerVerifier
makeIsDataIndexed ''RedeemerVerifier [('RedeemerVerifier, 0)]

-- TODO: split the setup data into the fixed and varying parts
-- | The Plutus script for verifying a ZkFold Symbolic smart contract.
{-# INLINABLE symbolicVerifier #-}
symbolicVerifier :: DatumVerifier -> RedeemerVerifier -> ScriptContext -> Bool
symbolicVerifier _ (RedeemerVerifier contract input proof) ctx =
    condition1 && condition2
    where
        info  = scriptContextTxInfo ctx

        ins   = txInfoInputs info
        refs  = txInfoReferenceInputs info
        outs  = txInfoOutputs info
        range = txInfoValidRange info

        hash = F . byteStringToInteger BigEndian . takeByteString 31 . blake2b_256 . serialiseData . toBuiltinData $ (ins, refs, outs, range)
        -- Verifying that the public input in the ZKP protocol corresponds to the hash of the transaction data.
        --
        -- ZkFold Symbolic smart contracts will have access to inputs, reference inputs, outputs and the transaction validity range.
        -- Other TxInfo fields can either be passed to the Symbolic contract as private inputs or are not particularly useful inside a contract.
        condition1 = pubInput input == negate hash

        -- Verifying the validity of the ZkFold Symbolic smart contract on the current transaction.
        -- The smart contract is encoded into the `Setup PlonkPlutus` data structure.
        condition2 = verify @PlonkPlutus contract input proof

-- | The Plutus script (minting policy) for verifying a Plonk proof.
{-# INLINABLE plonkVerifier #-}
plonkVerifier :: DatumVerifier -> RedeemerVerifier -> ScriptContext -> Bool
plonkVerifier _ (RedeemerVerifier computation input proof) ctx =
    condition0 && (condition1 || condition2)
    where
        info               = scriptContextTxInfo ctx

        Just m             = AssocMap.lookup (ownCurrencySymbol ctx) (getValue $ txInfoMint info)
        [(TokenName t, n)] = AssocMap.toList m

        -- With this minting policy, we can mint tokens if the Plonk proof is valid for the input provided in the redeemer.
        -- The tokens serve as proof that the network has verified the computation.
        -- We can also burn already minted tokens.

        -- Verifying that the token name equals to the bytestring representation of the public input in the ZKP protocol.
        condition0 = t == serialiseData (toBuiltinData input)

        -- Burning already minted tokens.
        condition1 = n < 0

        -- Verifying the Plonk proof.
        condition2 = verify @PlonkPlutus computation input proof
