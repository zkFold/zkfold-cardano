{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Asterizm where

import           Data.Coerce                 (coerce)
import           PlutusLedgerApi.V1          (flattenValue)
import           PlutusLedgerApi.V3          as V3
import           PlutusLedgerApi.V3.Contexts (ownCurrencySymbol)
import           PlutusTx                    (CompiledCode, compile)
import           PlutusTx.Prelude            (Bool (..), BuiltinUnit, Integer, Ord (..), blake2b_256, check, head, ($),
                                              (&&), (.), (<>), (==), (||))
import           PlutusTx.Trace              (traceError)

-- | Plutus script (minting policy) for posting signed messages on-chain.
{-# INLINABLE untypedAsterizmMessage #-}
untypedAsterizmMessage :: BuiltinData -> BuiltinUnit
untypedAsterizmMessage ctx' = check $ conditionCurrency && (conditionBurning || conditionVerifying)
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData ctx'

    info :: TxInfo
    info = scriptContextTxInfo ctx

    minted :: [(CurrencySymbol, TokenName, Integer)]
    minted = flattenValue . coerce . mintValueToMap $ txInfoMint info

    (cs, tn, amt) = case minted of
      [x] -> x
      _   -> traceError "Expected exactly one minting action"

    conditionCurrency  = cs == ownCurrencySymbol ctx

    conditionVerifying = tn == TokenName (blake2b_256 (getPubKeyHash pkh <> messageHash))
      where
        -- Redeemer expected as just a messageHash :: BuiltinByteString
        messageHash = unsafeFromBuiltinData . getRedeemer $ scriptContextRedeemer ctx

        -- Extract verification key from signatories
        pkh = case txInfoSignatories info of
          [s] -> s
          _   -> traceError "Expected exactly one signatory"

    conditionBurning = amt < 0 && case txOutDatum . head $ txInfoOutputs info of
                           -- Datum (of first TxOut) expected as original message
      OutputDatum d -> let message = unsafeFromBuiltinData $ getDatum d

                           -- Redeemer expected as relayer's pkh
                           pkh = unsafeFromBuiltinData . getRedeemer $ scriptContextRedeemer ctx
                       in  tn == TokenName (blake2b_256 (getPubKeyHash pkh <> blake2b_256 message))
      _             -> False


{-
untypedAsterizmMessage :: BuiltinData -> BuiltinUnit
untypedAsterizmMessage ctx =
  check $ conditionCurrency && conditionTokenName && (conditionBurning || conditionVerifying)
  where
      scriptContextTxInfo'     = BI.snd $ BI.unsafeDataAsConstr ctx
      scriptContextRedeemer'   = BI.tail scriptContextTxInfo'
      scriptContextScriptInfo' = BI.tail scriptContextRedeemer'

      cs'        = BI.head $ BI.snd $ BI.unsafeDataAsConstr $ BI.head scriptContextScriptInfo'
      info       = BI.head scriptContextTxInfo'
      txInfoMint = BI.head $ tail4 $ BI.snd $ BI.unsafeDataAsConstr info
      tail4      = BI.tail . BI.tail . BI.tail . BI.tail

      mints'             = BI.head $ BI.unsafeDataAsMap txInfoMint
      conditionCurrency  = BI.ifThenElse (BI.equalsData cs' $ BI.fst mints') True False

      ms = BI.unsafeDataAsMap $ BI.snd mints'
      m' = BI.head ms
      conditionTokenName = BI.ifThenElse (BI.null $ BI.tail ms) True False

      t = BI.unsafeDataAsB $ BI.fst m'
      n = BI.unsafeDataAsI $ BI.snd m'

      -- Extract message from ScriptContext
      message = BI.unsafeDataAsB $ BI.head scriptContextRedeemer'

      -- Extract signatory
      vk = BI.unsafeDataAsB $ BI.head $ BI.unsafeDataAsList $ BI.head $ tail4 $ tail4 $ BI.snd $ BI.unsafeDataAsConstr info

      -- Burning already minted tokens
      conditionBurning = n < 0

      -- Verifying message signature
      conditionVerifying = t == BI.blake2b_256 (vk <> message)
-}

asterizmMessageCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
asterizmMessageCompiled =
    $$(compile [|| untypedAsterizmMessage ||])
