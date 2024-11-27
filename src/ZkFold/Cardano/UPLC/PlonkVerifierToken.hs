{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.UPLC.PlonkVerifierToken where

import           PlutusTx                                 (UnsafeFromData (..))
import qualified PlutusTx.Builtins.Internal               as BI
import           PlutusTx.Prelude                         (Bool (..), BuiltinData, BuiltinUnit, Ord (..), check, ($),
                                                           (&&), (.), (||))

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain.BLS12_381.F       (toInput)
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes, SetupBytes)

-- | Plutus script (minting policy) for verifying computations on-chain.
--
-- The token is minted if and only if the Plonkup `proof` is valid for the `computation` on the `input` derived from the token name.
-- The computation is encoded into the token's currency symbol (aka policyID).
{-# INLINABLE untypedPlonkVerifierToken #-}
untypedPlonkVerifierToken :: SetupBytes -> BuiltinData -> BuiltinUnit
untypedPlonkVerifierToken computation ctx =
  check $ eqCs && (conditionBurning || conditionVerifying)
  where
      scriptContextTxInfo'     = BI.snd $ BI.unsafeDataAsConstr ctx
      scriptContextRedeemer'   = BI.tail scriptContextTxInfo'
      scriptContextScriptInfo' = BI.tail scriptContextRedeemer'

      cs'        = BI.head $ BI.snd $ BI.unsafeDataAsConstr $ BI.head scriptContextScriptInfo'
      info       = BI.head scriptContextTxInfo'
      txInfoMint = BI.head $ tail4 $ BI.snd $ BI.unsafeDataAsConstr info
      tail4      = BI.tail . BI.tail . BI.tail . BI.tail

      mints' = BI.head $ BI.unsafeDataAsMap txInfoMint
      eqCs   = BI.ifThenElse (BI.equalsData cs' $ BI.fst mints') True False
      m'     = BI.head $ BI.unsafeDataAsMap $ BI.snd mints'

      t = BI.unsafeDataAsB $ BI.fst m'
      n = BI.unsafeDataAsI $ BI.snd m'

      -- Extract redeemer from ScriptContext
      proof = unsafeFromBuiltinData @ProofBytes $ BI.head scriptContextRedeemer'

      -- Computing public input from the token name
      input = toInput t

      -- Burning already minted tokens
      conditionBurning = n < 0

      -- Verifying the Plonkup `proof` for the `computation` on `input`
      conditionVerifying = verify @PlonkupPlutus @HaskellCore computation input proof
