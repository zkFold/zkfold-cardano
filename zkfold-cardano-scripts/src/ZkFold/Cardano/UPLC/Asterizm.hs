{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Asterizm where

import           PlutusLedgerApi.V1.Value    (symbols, withCurrencySymbol)
import           PlutusLedgerApi.V3          as V3
import           PlutusLedgerApi.V3.Contexts (ownCurrencySymbol, txSignedBy)
import           PlutusTx                    (CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import           PlutusTx.AssocMap           (keys, lookup, toList)
import           PlutusTx.Prelude            hiding (toList)

type RelayerPKH = PubKeyHash

{-# INLINABLE buildCrosschainHash #-}
buildCrosschainHash :: BuiltinByteString -> BuiltinByteString
buildCrosschainHash bs =
  let headerLen = 112
      chunkLen  = 127
      len       = lengthOfByteString bs

      go :: BuiltinByteString -> Integer -> Integer -> BuiltinByteString
      go h off remLen =
        if remLen == 0 then h
        else
          let takeN = if remLen < chunkLen then remLen else chunkLen
              chunk = sliceByteString (headerLen + off) takeN bs
              h'    = sha2_256 (h <> sha2_256 chunk)
          in  go h' (off + takeN) (remLen - takeN)

  in  if len < headerLen then traceError "short"
      else
        let h0      = sha2_256 (sliceByteString 0 headerLen bs)
            payLen  = len - headerLen
        in go h0 0 payLen

-- | Plutus script (minting policy) for posting signed relayer messages (hashes) on-chain.
{-# INLINABLE untypedAsterizmRelayer #-}
untypedAsterizmRelayer :: RelayerPKH -> BuiltinData -> BuiltinUnit
untypedAsterizmRelayer pkh ctx' = check conditionSigned
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData ctx'

    info :: TxInfo
    info = scriptContextTxInfo ctx

    conditionSigned = txSignedBy info pkh

-- | Plutus script (minting policy) for posting actual messages on-chain.
-- When @isIncoming@ is True, validates relayer reference input (incoming cross-chain message).
-- When @isIncoming@ is False, skips relayer verification (outgoing cross-chain message).
{-# INLINABLE untypedAsterizmClient #-}
untypedAsterizmClient :: PubKeyHash -> [CurrencySymbol] -> Bool -> BuiltinData -> BuiltinUnit
untypedAsterizmClient clientPKH allowedRelayers True ctx' =
    let ctx = unsafeFromBuiltinData ctx'
        info = scriptContextTxInfo ctx
        refInputs = txInInfoResolved <$> txInfoReferenceInputs info
        valueReferenced = foldMap txOutValue refInputs
        minted = fmapDefault toList . lookup (ownCurrencySymbol ctx) . mintValueToMap $ txInfoMint info
        (tn, _) = case minted of
          Just [x] -> x
          _        -> traceError "Expected exactly one minting action"
        message = case txOutDatum . head $ txInfoOutputs info of
          OutputDatum d -> unsafeFromBuiltinData $ getDatum d
          _             -> traceError "Expected output datum"
        tokenName = TokenName $ buildCrosschainHash message
        conditionSigned = txSignedBy info clientPKH
        conditionMinting = tn == tokenName
        relayerCS = case find (\s -> s /= adaSymbol && s `elem` allowedRelayers) $ symbols valueReferenced of
          Just cs -> cs
          Nothing -> traceError "Unrecognized relayer"
        conditionVerifying = withCurrencySymbol relayerCS valueReferenced False $ \tokensMap ->
           head (keys tokensMap) == tokenName
    in check $ conditionSigned && conditionMinting && conditionVerifying

untypedAsterizmClient clientPKH _ False ctx' =
    let ctx = unsafeFromBuiltinData ctx'
        info = scriptContextTxInfo ctx
        minted = fmapDefault toList . lookup (ownCurrencySymbol ctx) . mintValueToMap $ txInfoMint info
        (tn, _) = case minted of
          Just [x] -> x
          _        -> traceError "Expected exactly one minting action"
        message = case txOutDatum . head $ txInfoOutputs info of
          OutputDatum d -> unsafeFromBuiltinData $ getDatum d
          _             -> traceError "Expected output datum"
        tokenName = TokenName $ buildCrosschainHash message
        conditionSigned = txSignedBy info clientPKH
        conditionMinting = tn == tokenName
    in check $ conditionSigned && conditionMinting

asterizmRelayerCompiled :: RelayerPKH -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmRelayerCompiled pkh =
    $$(compile [|| untypedAsterizmRelayer ||])
    `unsafeApplyCode` liftCodeDef pkh

asterizmClientCompiled :: PubKeyHash -> [CurrencySymbol] -> Bool -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmClientCompiled clientPKH allowedRelayers isIncoming =
    $$(compile [|| untypedAsterizmClient ||])
    `unsafeApplyCode` liftCodeDef clientPKH
    `unsafeApplyCode` liftCodeDef allowedRelayers
    `unsafeApplyCode` liftCodeDef isIncoming
