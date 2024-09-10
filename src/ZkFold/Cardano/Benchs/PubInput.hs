module ZkFold.Cardano.Benchs.PubInput where

import           GHC.ByteOrder                            (ByteOrder(..))
import           PlutusLedgerApi.V3                       (ScriptContext (..))
import           PlutusTx.Builtins                        (byteStringToInteger, greaterThanInteger)
import           PlutusTx.Prelude                         (Bool (..), Integer, ($), (.))

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain.Data        (InputBytes, ProofBytes, SetupBytes)
import           ZkFold.Cardano.Plonk.OnChain.Utils       (dataToBlake, toInput)


-- ZkFold Symbolic smart contracts have access to inputs, reference inputs, outputs and the transaction validity range.

{-# INLINABLE pubInput #-}
pubInput :: ScriptContext -> Bool
pubInput ctx = greaterThanInteger input 0
    where
        redm = scriptContextRedeemer ctx

        -- Computing public input from the transaction data
        input = byteStringToInteger BigEndian . dataToBlake $ redm

{-# INLINABLE symbolicVerifierBench1 #-}
symbolicVerifierBench1 :: SetupBytes -> ProofBytes -> ScriptContext -> Bool
symbolicVerifierBench1 contract proof _ =
    -- Verifying the Plonk `proof` for the `contract` on the transaction data encoded as `input`
    verify @PlonkPlutus contract input proof
    where
        -- Computing public input from the transaction data
        input = toInput $ dataToBlake (0 :: Integer)
