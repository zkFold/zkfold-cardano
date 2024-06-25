{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.PlonkVerifier where

import           GHC.ByteOrder                            (ByteOrder (..))
import           PlutusLedgerApi.V1.Value                 (Value (..))
import           PlutusLedgerApi.V3                       (ScriptContext(..), TokenName (..), TxInfo (..))
import           PlutusLedgerApi.V3.Contexts              (ownCurrencySymbol)
import qualified PlutusTx.AssocMap                        as AssocMap
import           PlutusTx.Builtins                        (byteStringToInteger)
import           PlutusTx.Prelude                         (Bool (..), Maybe (..), Ord (..), ($), (.), (||))

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (InputBytes (..), ProofBytes, SetupBytes, toF)

-- | The Plutus script (minting policy) for verifying computations on-chain.
--
-- The token is minted if and only if the Plonk `proof` is valid for the `computation` on the `input` derived from the token name.
-- The computation is encoded into the token's currency symbol (aka policyID).
{-# INLINABLE plonkVerifier #-}
plonkVerifier :: SetupBytes -> ProofBytes -> ScriptContext -> Bool
plonkVerifier computation proof ctx =
    conditionBurning || conditionVerifying
    where
        info               = scriptContextTxInfo ctx

        -- Finding own tokens
        Just m             = AssocMap.lookup (ownCurrencySymbol ctx) (getValue $ txInfoMint info)
        [(TokenName t, n)] = AssocMap.toList m
        
        -- Computing public input from the token name
        input = InputBytes . toF . byteStringToInteger BigEndian $ t

        -- Burning already minted tokens
        conditionBurning   = n < 0

        -- Verifying the Plonk `proof` for the `computation` on `input`
        conditionVerifying = verify @PlonkPlutus computation input proof
