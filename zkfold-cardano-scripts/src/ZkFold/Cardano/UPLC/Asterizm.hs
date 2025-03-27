{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Asterizm where

import           PlutusTx                   (CompiledCode, compile)
import qualified PlutusTx.Builtins.Internal as BI
import           PlutusTx.Prelude           (Bool (..), BuiltinData, BuiltinUnit, Ord (..), check, ($), (&&), (.), (<>),
                                             (==), (||))

-- | Plutus script (minting policy) for posting signed messages on-chain.
{-# INLINABLE untypedAsterizmMessage #-}
untypedAsterizmMessage :: BuiltinData -> BuiltinUnit
untypedAsterizmMessage ctx =
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

      -- Extract message from ScriptContext
      message = BI.unsafeDataAsB $ BI.head scriptContextRedeemer'

      -- Hash message
      hash    = BI.sha2_256 message

      -- Extract signatory
      vk = BI.unsafeDataAsB $ BI.head $ BI.unsafeDataAsList $ BI.head $ tail4 $ tail4 $ BI.snd $ BI.unsafeDataAsConstr info

      -- Burning already minted tokens
      conditionBurning = n < 0

      -- Verifying message signature
      conditionVerifying = t == vk <> hash

asterizmMessageCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
asterizmMessageCompiled =
    $$(compile [|| untypedAsterizmMessage ||])
