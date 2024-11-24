{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.UPLC.PlonkVerifierToken where

import           PlutusLedgerApi.V1.Value                 (Value (..))
import           PlutusLedgerApi.V3                       (ScriptContext (..), TokenName (..), TxInfo (..), getRedeemer)
import           PlutusLedgerApi.V3.Contexts              (ownCurrencySymbol)
import           PlutusTx                                 (UnsafeFromData (..))
import qualified PlutusTx.AssocMap                        as AssocMap
import           PlutusTx.Prelude                         (Bool (..), BuiltinData, BuiltinUnit, Maybe (..), Ord (..),
                                                           check, ($), (.), (||))

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain.BLS12_381.F       (toInput)
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes, SetupBytes)

-- | Plutus script (minting policy) for verifying computations on-chain.
--
-- The token is minted if and only if the Plonkup `proof` is valid for the `computation` on the `input` derived from the token name.
-- The computation is encoded into the token's currency symbol (aka policyID).
{-# INLINABLE plonkVerifierToken #-}
plonkVerifierToken :: SetupBytes -> ProofBytes -> ScriptContext -> Bool
plonkVerifierToken computation proof ctx =
    conditionBurning || conditionVerifying
    where
        mints              = getValue $ txInfoMint $ scriptContextTxInfo ctx

        -- Finding own tokens
        Just m             = AssocMap.lookup (ownCurrencySymbol ctx) mints
        [(TokenName t, n)] = AssocMap.toList m

        -- Computing public input from the token name
        input              = toInput t

        -- Burning already minted tokens
        conditionBurning   = n < 0

        -- Verifying the Plonkup `proof` for the `computation` on `input`
        conditionVerifying = verify @PlonkupPlutus @HaskellCore computation input proof

{-# INLINABLE untypedPlonkVerifierToken #-}
untypedPlonkVerifierToken :: SetupBytes -> BuiltinData -> BuiltinUnit
untypedPlonkVerifierToken computation' ctx' =
  let
    ctx           = unsafeFromBuiltinData ctx'
    redeemerProof = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ plonkVerifierToken computation' redeemerProof ctx
