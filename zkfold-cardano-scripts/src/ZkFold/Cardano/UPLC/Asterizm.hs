{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Asterizm where

import           PlutusLedgerApi.V1          (currencySymbolValueOf, symbols)
import           PlutusLedgerApi.V1.Value    (withCurrencySymbol)
import           PlutusLedgerApi.V3          as V3
import           PlutusLedgerApi.V3.Contexts (ownCurrencySymbol, txSignedBy)
import           PlutusTx                    (CompiledCode, compile, liftCodeDef, makeIsDataIndexed, makeLift,
                                              unsafeApplyCode)
import           PlutusTx.AssocMap           (keys, lookup, toList)
import           PlutusTx.Builtins           (lengthOfByteString, sha2_256, sliceByteString)
import           PlutusTx.Foldable           (foldMap)
import           PlutusTx.Prelude            (Bool (..), BuiltinUnit, Integer, Maybe (..), Ord (..), blake2b_256, check,
                                              elem, find, fmapDefault, head, ($), (&&), (.), (/=), (<$>), (==), (||),
                                              (<>), (+), (-))
import           PlutusTx.Trace              (traceError)


type RelayerPKH = PubKeyHash

-- | Setup parameters for @asterizmClient@
data AsterizmSetup = AsterizmSetup
  { acsClientPKH    :: PubKeyHash
  , acsThreadSymbol :: CurrencySymbol
  }

makeLift ''AsterizmSetup
makeIsDataIndexed ''AsterizmSetup [('AsterizmSetup,0)]


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

-- | Plutus script (minting policy) for posting signed messages on-chain.
{-# INLINABLE untypedAsterizmRelayer #-}
untypedAsterizmRelayer :: RelayerPKH -> BuiltinData -> BuiltinUnit
untypedAsterizmRelayer pkh ctx' = check $ conditionSigned && (conditionBurning || conditionVerifying)
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData ctx'

    info :: TxInfo
    info = scriptContextTxInfo ctx

    minted :: Maybe [(TokenName, Integer)]
    minted = fmapDefault toList . lookup (ownCurrencySymbol ctx) . mintValueToMap $ txInfoMint info

    (tn, amt) = case minted of
      Just [x] -> x
      _        -> traceError "Expected exactly one minting action"

    messageHash :: BuiltinByteString
    messageHash = unsafeFromBuiltinData . getRedeemer $ scriptContextRedeemer ctx

    conditionSigned = txSignedBy info pkh

    conditionBurning = amt < 0

    conditionVerifying = lengthOfByteString messageHash == 32 && tn == TokenName messageHash


-- | Plutus script (minting policy) for posting actual messages on-chain.
{-# INLINABLE untypedAsterizmClient #-}
untypedAsterizmClient :: AsterizmSetup -> BuiltinData -> BuiltinUnit
untypedAsterizmClient AsterizmSetup{..} ctx' = check $ conditionSigned &&
    (conditionBurning || conditionMinting && conditionVerifying)
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData ctx'

    info :: TxInfo
    info = scriptContextTxInfo ctx

    refInputs :: [TxOut]
    refInputs = txInInfoResolved <$> txInfoReferenceInputs info

    threadInput :: TxOut
    threadInput = case find (\i -> currencySymbolValueOf (txOutValue i) acsThreadSymbol == 1)
                       refInputs of
      Just out -> out
      Nothing  -> traceError "Missing thread token."

    valueReferenced :: Value
    valueReferenced = foldMap txOutValue refInputs

    minted :: Maybe [(TokenName, Integer)]
    minted = fmapDefault toList . lookup (ownCurrencySymbol ctx) . mintValueToMap $ txInfoMint info

    (tn, amt) = case minted of
      Just [x] -> x
      _        -> traceError "Expected exactly one minting action"

    message :: BuiltinByteString
    message = case txOutDatum . head $ txInfoOutputs info of
      OutputDatum d -> unsafeFromBuiltinData $ getDatum d
      _             -> traceError "Expected output datum"

    relayers :: [CurrencySymbol]
    relayers = case txOutDatum threadInput of
                 OutputDatum d -> unsafeFromBuiltinData $ getDatum d
                 _             -> traceError "Missing registry"

    relayerCS :: CurrencySymbol
    relayerCS = case find (\s -> s /= adaSymbol && s `elem` relayers) $ symbols
                     valueReferenced of
      Just cs -> cs
      Nothing -> traceError "Unrecognized relayer"

    tokenName = TokenName $ buildCrosschainHash message

    conditionSigned = txSignedBy info acsClientPKH

    conditionBurning = amt < 0

    conditionMinting = tn == tokenName

    conditionVerifying = withCurrencySymbol relayerCS valueReferenced False $ \tokensMap ->
      head (keys tokensMap) == tokenName


asterizmRelayerCompiled :: RelayerPKH -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmRelayerCompiled pkh =
    $$(compile [|| untypedAsterizmRelayer ||])
    `unsafeApplyCode` liftCodeDef pkh

asterizmClientCompiled :: AsterizmSetup -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmClientCompiled setup =
    $$(compile [|| untypedAsterizmClient ||])
    `unsafeApplyCode` liftCodeDef setup
