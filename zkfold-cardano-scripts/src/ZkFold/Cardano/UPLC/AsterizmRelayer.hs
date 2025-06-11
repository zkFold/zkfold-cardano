{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.AsterizmRelayer where

import           Data.Maybe                  (fromJust)
import           PlutusLedgerApi.V3          as V3
import           PlutusLedgerApi.V3.Contexts (ownCurrencySymbol, txSignedBy)
import           PlutusTx                    (CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import           PlutusTx.AssocMap           (lookup, toList)
import           PlutusTx.Prelude            (BuiltinUnit, Integer, Ord (..), check, lengthOfByteString, ($), (&&), (.),
                                              (==), (||))
import           PlutusTx.Trace              (traceError)


type RelayerPKH = PubKeyHash

-- | Plutus script (minting policy) for posting signed messages on-chain.
{-# INLINABLE untypedAsterizmMessage #-}
untypedAsterizmMessage :: RelayerPKH -> BuiltinData -> BuiltinUnit
untypedAsterizmMessage pkh ctx' = check $ conditionSigned && (conditionBurning || conditionVerifying)
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData ctx'

    info :: TxInfo
    info = scriptContextTxInfo ctx

    minted :: [(TokenName, Integer)]
    minted = toList . fromJust . lookup (ownCurrencySymbol ctx) . mintValueToMap $ txInfoMint info

    (tn, amt) = case minted of
      [x] -> x
      _   -> traceError "Expected exactly one minting action"

    messageHash :: BuiltinByteString
    messageHash = unsafeFromBuiltinData . getRedeemer $ scriptContextRedeemer ctx

    conditionBurning = amt < 0

    conditionSigned = txSignedBy info pkh

    conditionVerifying = lengthOfByteString messageHash == 32 && tn == TokenName messageHash

asterizmMessageCompiled :: RelayerPKH -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmMessageCompiled pkh =
    $$(compile [|| untypedAsterizmMessage ||])
    `unsafeApplyCode` liftCodeDef pkh
