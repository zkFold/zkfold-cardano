module ZkFold.Cardano.Benchs.PubInput where

import           GHC.ByteOrder                            (ByteOrder(..))
import           PlutusLedgerApi.V3                       (ScriptContext (..))
import           PlutusTx.Builtins                        (byteStringToInteger, greaterThanInteger)
import           PlutusTx.Prelude                         (Bool (..), Integer, ($), (.))

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain.Data        (ProofBytes, SetupBytes)
import           ZkFold.Cardano.Plonk.OnChain.Utils       (dataToBlake, toInput)


-- | We use 'pubInput' to estimate the exec units cost of (blake2b_224) hashing
-- the data passed in redeemer, and subsequent conversion to Integer.  By
-- controlling byte-size of redeemer we can obtain a model for hashing cost.

{-# INLINABLE pubInput #-}
pubInput :: ScriptContext -> Bool
pubInput ctx = greaterThanInteger input 0
    where
        redm = scriptContextRedeemer ctx

        -- Computing public input from the transaction data
        input = byteStringToInteger BigEndian . dataToBlake $ redm

-- | We use 'symbolicVerifierBench' to estimate exec units associated to
-- execution of 'verify @PlonkPlutus'.  Note that "input" size, being a hash
-- digest, is constant.

{-# INLINABLE symbolicVerifierBench1 #-}
symbolicVerifierBench1 :: SetupBytes -> ProofBytes -> ScriptContext -> Bool
symbolicVerifierBench1 contract proof _ =
    -- Verifying the Plonk `proof` for the `contract` on the transaction data encoded as `input`
    verify @PlonkPlutus contract input proof
    where
        -- Computing public input from the transaction data
        input = toInput $ dataToBlake (0 :: Integer)
