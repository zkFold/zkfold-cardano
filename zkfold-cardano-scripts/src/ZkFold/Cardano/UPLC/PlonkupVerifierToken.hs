{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.PlonkupVerifierToken where

import           PlutusTx                            (CompiledCode, UnsafeFromData (..), compile, liftCodeDef,
                                                      unsafeApplyCode)
import qualified PlutusTx.Builtins.Internal          as BI
import           PlutusTx.Prelude                    (Bool (..), BuiltinData, BuiltinUnit, Ord (..), check, ($), (&&),
                                                      (.), (||))

import           ZkFold.Cardano.OnChain.BLS12_381.F  (toInput)
import           ZkFold.Cardano.OnChain.Plonkup      (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes, SetupBytes)
import           ZkFold.Protocol.NonInteractiveProof (NonInteractiveProof (..))

-- | Plutus script (minting policy) for verifying computations on-chain.
--
-- The token is minted if and only if the Plonkup `proof` is valid for the `computation` on the `input` derived from the token name.
-- The computation is encoded into the token's currency symbol (aka policyID).
{-# INLINABLE untypedPlonkupVerifierToken #-}
untypedPlonkupVerifierToken :: SetupBytes -> BuiltinData -> BuiltinUnit
untypedPlonkupVerifierToken computation ctx =
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
      conditionVerifying = verify @PlonkupPlutus computation [input] proof

plonkupVerifierTokenCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
plonkupVerifierTokenCompiled computation =
    $$(compile [|| untypedPlonkupVerifierToken ||])
    `unsafeApplyCode` liftCodeDef computation
