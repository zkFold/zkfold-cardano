{-# LANGUAGE TemplateHaskell #-}

module ZkFold.Cardano.ScriptsWithParams where

import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V1.Interval              (contains, to)
import           PlutusLedgerApi.V3                       (POSIXTime, ScriptContext (..), TxInfo (..))
import           PlutusTx                                 (makeIsDataIndexed, makeLift, toBuiltinData)
import           PlutusTx.Builtins                        (BuiltinByteString, blake2b_256, serialiseData)
import           PlutusTx.Prelude                         (Bool (..), Eq (..), ($), (&&), (.))

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (InputBytes, ProofBytes, SetupBytes)

data ParamsProof = ParamsProof {
    hash    :: BuiltinByteString
  , endTime :: POSIXTime
} deriving stock (Generic)

makeLift ''ParamsProof
makeIsDataIndexed ''ParamsProof [('ParamsProof, 0)]

data DatumProof = DatumProof
  deriving stock (Generic)

makeLift ''DatumProof
makeIsDataIndexed ''DatumProof [('DatumProof, 0)]

data RedeemerProof = RedeemerProof SetupBytes InputBytes ProofBytes
  deriving stock (Generic)

makeLift ''RedeemerProof
makeIsDataIndexed ''RedeemerProof [('RedeemerProof, 0)]

-- TODO: split the setup data into the fixed and varying parts
-- | The Plutus script for verifying a ZkFold Symbolic smart contract.
{-# INLINABLE delegateProof #-}
delegateProof :: ParamsProof -> DatumProof -> RedeemerProof -> ScriptContext -> Bool
delegateProof (ParamsProof hash contractEndTime) _ (RedeemerProof contract input proof) ctx =
    condition0 && condition1 && condition2
    where
        info  = scriptContextTxInfo ctx

        ins   = txInfoInputs info
        outs  = txInfoOutputs info
        refs  = txInfoReferenceInputs info

        -- Contract still available.
        condition0 = to contractEndTime `contains` txInfoValidRange info

        -- Verifying that the public input in the ZKP protocol corresponds to the hash of the transaction data.
        --
        -- ZkFold Symbolic smart contracts will have access to inputs, reference inputs, outputs and the transaction validity range.
        -- Other TxInfo fields can either be passed to the Symbolic contract as private inputs or are not particularly useful inside a contract.
        condition1 = hash == (blake2b_256 . serialiseData . toBuiltinData $ (ins, refs, outs))

        -- Verifying the validity of the ZkFold Symbolic smart contract on the current transaction.
        -- The smart contract is encoded into the `Setup PlonkPlutus` data structure.
        condition2 = verify @PlonkPlutus contract input proof
