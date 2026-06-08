{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Asterizm where

import           GHC.ByteOrder               (ByteOrder (..))
import           GHC.Generics                 (Generic)
import           PlutusLedgerApi.V1.Value    (symbols, valueOf, withCurrencySymbol)
import           PlutusLedgerApi.V3          as V3
import           PlutusLedgerApi.V3.Contexts (ownCurrencySymbol, txSignedBy)
import           PlutusTx                    (CompiledCode, compile, liftCodeDef, makeIsDataIndexed, unsafeApplyCode)
import           PlutusTx.AssocMap           (keys, lookup, toList)
import           PlutusTx.Builtins           (replicateByte)
import           PlutusTx.Prelude            hiding (toList)
import           Prelude                     (Show)

type RelayerPKH = PubKeyHash
type TrustedAddress = BuiltinByteString

data AsterizmHashMode =
    RegularHash
  | CrosschainHash
  deriving stock (Show, Generic)

makeIsDataIndexed ''AsterizmHashMode [('RegularHash,0),('CrosschainHash,1)]

data AsterizmClientAction =
    ClientIncoming AsterizmHashMode
  | ClientOutgoing AsterizmHashMode
  deriving stock (Show, Generic)

makeIsDataIndexed ''AsterizmClientAction [('ClientIncoming,0),('ClientOutgoing,1)]

data AsterizmOmniTokenAction =
    OmniTokenMint
  | OmniTokenBurn
  deriving stock (Show, Generic)

makeIsDataIndexed ''AsterizmOmniTokenAction [('OmniTokenMint,0),('OmniTokenBurn,1)]

{-# INLINABLE buildHash #-}
buildHash :: BuiltinByteString -> BuiltinByteString
buildHash = sha2_256

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

{-# INLINABLE buildAsterizmHash #-}
buildAsterizmHash :: AsterizmHashMode -> BuiltinByteString -> BuiltinByteString
buildAsterizmHash RegularHash = buildHash
buildAsterizmHash CrosschainHash = buildCrosschainHash

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
-- Incoming messages validate a relayer reference input.
-- Outgoing messages validate and burn a user approval token.
{-# INLINABLE untypedAsterizmClient #-}
untypedAsterizmClient :: PubKeyHash -> [CurrencySymbol] -> CurrencySymbol -> [TrustedAddress] -> BuiltinData -> BuiltinUnit
untypedAsterizmClient clientPKH allowedRelayers userCS trustedAddresses ctx' =
    let ctx = unsafeFromBuiltinData ctx'
        action = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
        info = scriptContextTxInfo ctx
        minted = fmapDefault toList . lookup (ownCurrencySymbol ctx) . mintValueToMap $ txInfoMint info
        (tn, _) = case minted of
          Just [x] -> x
          _        -> traceError "Expected exactly one minting action"
        message = case txOutDatum . head $ txInfoOutputs info of
          OutputDatum d -> unsafeFromBuiltinData $ getDatum d
          _             -> traceError "Expected output datum"
        conditionSigned = txSignedBy info clientPKH
    in case action of
      ClientIncoming hashMode ->
        let refInputs = txInInfoResolved <$> txInfoReferenceInputs info
            valueReferenced = foldMap txOutValue refInputs
            tokenName = TokenName $ buildAsterizmHash hashMode message
            conditionMinting = tn == tokenName
            conditionSourceTrusted = trustedSource trustedAddresses message
            conditionDestinationClient = clientDestination ctx message
            relayerCS = case find (\s -> s /= adaSymbol && s `elem` allowedRelayers) $ symbols valueReferenced of
              Just cs -> cs
              Nothing -> traceError "Unrecognized relayer"
            conditionVerifying = hasToken relayerCS tokenName valueReferenced
        in check $ conditionSigned && conditionMinting && conditionSourceTrusted && conditionDestinationClient && conditionVerifying
      ClientOutgoing hashMode ->
        let inputs = txInInfoResolved <$> txInfoInputs info
            valueSpent = foldMap txOutValue inputs
            tokenName = TokenName $ buildAsterizmHash hashMode message
            conditionMinting = tn == tokenName
            conditionUserApproved = hasToken userCS tokenName valueSpent
            conditionUserBurned = tokenMintAmount userCS tokenName info == negate 1
            conditionSourceClient = clientSource ctx message
            conditionDestinationTrusted = trustedDestination trustedAddresses message
        in check $ conditionSigned && conditionMinting && conditionUserApproved && conditionUserBurned && conditionSourceClient && conditionDestinationTrusted

{-# INLINABLE hasToken #-}
hasToken :: CurrencySymbol -> TokenName -> Value -> Bool
hasToken cs tn value =
  withCurrencySymbol cs value False $ \tokensMap -> tn `elem` keys tokensMap

{-# INLINABLE trustedSource #-}
trustedSource :: [TrustedAddress] -> BuiltinByteString -> Bool
trustedSource trustedAddresses message =
  sliceByteString 0 40 message `elem` trustedAddresses

{-# INLINABLE trustedDestination #-}
trustedDestination :: [TrustedAddress] -> BuiltinByteString -> Bool
trustedDestination trustedAddresses message =
  sliceByteString 40 40 message `elem` trustedAddresses

{-# INLINABLE clientSource #-}
clientSource :: ScriptContext -> BuiltinByteString -> Bool
clientSource ctx message =
  sliceByteString 8 32 message == clientAddress ctx

{-# INLINABLE clientDestination #-}
clientDestination :: ScriptContext -> BuiltinByteString -> Bool
clientDestination ctx message =
  sliceByteString 48 32 message == clientAddress ctx

{-# INLINABLE clientAddress #-}
clientAddress :: ScriptContext -> BuiltinByteString
clientAddress ctx = leftPad32 . unCurrencySymbol $ ownCurrencySymbol ctx

{-# INLINABLE leftPad32 #-}
leftPad32 :: BuiltinByteString -> BuiltinByteString
leftPad32 bs =
  let len = lengthOfByteString bs
  in if len > 32
     then traceError "Address too long"
     else replicateByte (32 - len) 0 <> bs

asterizmRelayerCompiled :: RelayerPKH -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmRelayerCompiled pkh =
    $$(compile [|| untypedAsterizmRelayer ||])
    `unsafeApplyCode` liftCodeDef pkh

asterizmClientCompiled :: PubKeyHash -> [CurrencySymbol] -> CurrencySymbol -> [TrustedAddress] -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmClientCompiled clientPKH allowedRelayers userCS trustedAddresses =
    $$(compile [|| untypedAsterizmClient ||])
    `unsafeApplyCode` liftCodeDef clientPKH
    `unsafeApplyCode` liftCodeDef allowedRelayers
    `unsafeApplyCode` liftCodeDef userCS
    `unsafeApplyCode` liftCodeDef trustedAddresses

-- | Plutus script (minting policy) for posting user messages on-chain.
-- Unlike the client policy, this policy is not parameterized by any public key hash.
-- It only validates that the token name matches the cross-chain hash of the message.
{-# INLINABLE untypedAsterizmUser #-}
untypedAsterizmUser :: BuiltinData -> BuiltinUnit
untypedAsterizmUser ctx' =
    let ctx = unsafeFromBuiltinData ctx'
        hashMode = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
        info = scriptContextTxInfo ctx
        minted = fmapDefault toList . lookup (ownCurrencySymbol ctx) . mintValueToMap $ txInfoMint info
        (tn, _) = case minted of
          Just [x] -> x
          _        -> traceError "Expected exactly one minting action"
        message = case txOutDatum . head $ txInfoOutputs info of
          OutputDatum d -> unsafeFromBuiltinData $ getDatum d
          _             -> traceError "Expected output datum"
        tokenName = TokenName $ buildAsterizmHash hashMode message
        conditionMinting = tn == tokenName
    in check conditionMinting

asterizmUserCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
asterizmUserCompiled = $$(compile [|| untypedAsterizmUser ||])

{-# INLINABLE omniTokenName #-}
omniTokenName :: TokenName
omniTokenName = TokenName emptyByteString

{-# INLINABLE asterizmTokenPayloadLen #-}
asterizmTokenPayloadLen :: Integer
asterizmTokenPayloadLen = 96

{-# INLINABLE asterizmTokenMessageLen #-}
asterizmTokenMessageLen :: Integer
asterizmTokenMessageLen = 208

{-# INLINABLE asterizmHeaderTxId #-}
asterizmHeaderTxId :: BuiltinByteString -> BuiltinByteString
asterizmHeaderTxId = sliceByteString 80 32

{-# INLINABLE asterizmTokenPayload #-}
asterizmTokenPayload :: BuiltinByteString -> BuiltinByteString
asterizmTokenPayload message =
  if lengthOfByteString message == asterizmTokenMessageLen
  then sliceByteString 112 asterizmTokenPayloadLen message
  else traceError "Invalid omni token message length"

{-# INLINABLE asterizmTokenDstAddress #-}
asterizmTokenDstAddress :: BuiltinByteString -> BuiltinByteString
asterizmTokenDstAddress payload = sliceByteString 0 32 payload

{-# INLINABLE asterizmTokenAmount #-}
asterizmTokenAmount :: BuiltinByteString -> Integer
asterizmTokenAmount payload = byteStringToInteger BigEndian $ sliceByteString 32 32 payload

{-# INLINABLE asterizmTokenPayloadTxId #-}
asterizmTokenPayloadTxId :: BuiltinByteString -> BuiltinByteString
asterizmTokenPayloadTxId payload = sliceByteString 64 32 payload

{-# INLINABLE ownTokenAmount #-}
ownTokenAmount :: CurrencySymbol -> TokenName -> TxInfo -> Integer
ownTokenAmount cs tn info = case lookup cs . mintValueToMap $ txInfoMint info of
  Just xs -> case toList xs of
    [(tn', amount)] ->
      if tn' == tn
      then amount
      else traceError "Unexpected token name"
    _ -> traceError "Expected exactly one token minting action"
  _ -> traceError "Expected token minting action"

{-# INLINABLE mintedToken #-}
mintedToken :: CurrencySymbol -> TxInfo -> (TokenName, Integer)
mintedToken cs info = case lookup cs . mintValueToMap $ txInfoMint info of
  Just xs -> case toList xs of
    [x] -> x
    _   -> traceError "Expected exactly one proof minting action"
  _ -> traceError "Expected proof minting action"

{-# INLINABLE tokenMintAmount #-}
tokenMintAmount :: CurrencySymbol -> TokenName -> TxInfo -> Integer
tokenMintAmount cs tn info = case lookup cs . mintValueToMap $ txInfoMint info of
  Just xs -> case find (\(tn', _) -> tn' == tn) $ toList xs of
    Just (_, amount) -> amount
    _                -> 0
  _ -> 0

{-# INLINABLE proofMessage #-}
proofMessage :: CurrencySymbol -> TokenName -> [TxOut] -> BuiltinByteString
proofMessage cs tn outputs = case find (\o -> valueOf (txOutValue o) cs tn == 1) outputs of
  Just o -> case txOutDatum o of
    OutputDatum d -> unsafeFromBuiltinData $ getDatum d
    _             -> traceError "Expected proof output datum"
  _ -> traceError "Expected proof output"

{-# INLINABLE paysToPaymentKeyHash #-}
paysToPaymentKeyHash :: BuiltinByteString -> TxOut -> Bool
paysToPaymentKeyHash pkh out = case txOutAddress out of
  Address (PubKeyCredential pkh') _ -> leftPad32 (getPubKeyHash pkh') == pkh
  _                                 -> False

{-# INLINABLE valuePaidToPaymentKeyHash #-}
valuePaidToPaymentKeyHash :: CurrencySymbol -> TokenName -> BuiltinByteString -> [TxOut] -> Integer
valuePaidToPaymentKeyHash cs tn pkh outputs = case outputs of
  [] -> 0
  o : os ->
    let rest = valuePaidToPaymentKeyHash cs tn pkh os
    in if paysToPaymentKeyHash pkh o
       then valueOf (txOutValue o) cs tn + rest
       else rest

{-# INLINABLE hasInputWithTokens #-}
hasInputWithTokens :: CurrencySymbol -> TokenName -> CurrencySymbol -> TokenName -> Integer -> [TxInInfo] -> Bool
hasInputWithTokens userCS userTN omniCS omniTN amount inputs = case inputs of
  [] -> False
  i : is ->
    let v = txOutValue $ txInInfoResolved i
    in  (valueOf v userCS userTN == 1 && valueOf v omniCS omniTN >= amount)
        || hasInputWithTokens userCS userTN omniCS omniTN amount is

{-# INLINABLE untypedAsterizmOmniToken #-}
untypedAsterizmOmniToken :: CurrencySymbol -> CurrencySymbol -> BuiltinData -> BuiltinUnit
untypedAsterizmOmniToken clientCS userCS ctx' =
    let ctx = unsafeFromBuiltinData ctx'
        action = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
        info = scriptContextTxInfo ctx
        ownCS = ownCurrencySymbol ctx
        ownAmount = ownTokenAmount ownCS omniTokenName info
        (proofTN, proofAmount) = mintedToken clientCS info
        message = proofMessage clientCS proofTN $ txInfoOutputs info
        payload = asterizmTokenPayload message
        dstAddress = asterizmTokenDstAddress payload
        amount = asterizmTokenAmount payload
        payloadTxId = asterizmTokenPayloadTxId payload
        conditionProofMinted = proofAmount == 1
        conditionAmountPositive = amount > 0
        conditionTxId = payloadTxId == asterizmHeaderTxId message
        conditionMint = ownAmount == amount
          && valuePaidToPaymentKeyHash ownCS omniTokenName dstAddress (txInfoOutputs info) >= amount
        conditionBurn = ownAmount == negate amount
          && tokenMintAmount userCS proofTN info == negate 1
          && hasInputWithTokens userCS proofTN ownCS omniTokenName amount (txInfoInputs info)
        conditionAction = case action of
          OmniTokenMint -> conditionMint
          OmniTokenBurn -> conditionBurn
    in check $ conditionProofMinted && conditionAmountPositive && conditionTxId && conditionAction

asterizmOmniTokenCompiled :: CurrencySymbol -> CurrencySymbol -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmOmniTokenCompiled clientCS userCS =
    $$(compile [|| untypedAsterizmOmniToken ||])
    `unsafeApplyCode` liftCodeDef clientCS
    `unsafeApplyCode` liftCodeDef userCS
