{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Asterizm where

-- import           PlutusLedgerApi.V1          (symbols)
import           PlutusLedgerApi.V1.Value    (currencySymbolValueOf, lovelaceValueOf, valueOf, withCurrencySymbol, symbols)
import           PlutusLedgerApi.V3          as V3
import           PlutusLedgerApi.V3.Contexts (findOwnInput, getContinuingOutputs, ownCurrencySymbol, txSignedBy)
import           PlutusTx                    (CompiledCode, compile, liftCodeDef, makeIsDataIndexed, makeLift,
                                              unsafeApplyCode)
import           PlutusTx.AssocMap           (keys, lookup, toList)
-- import qualified PlutusTx.Foldable           as F
-- import qualified PlutusTx.Data.List          as L
import           PlutusTx.Prelude            hiding (toList)

type RelayerPKH = PubKeyHash

-- | Setup parameters for Asterizm's admin
data AsterizmAdmin = AsterizmAdmin
  { aaAsterizmPKH :: PubKeyHash
  , aaAsterizmFee :: Lovelace
  }

makeLift ''AsterizmAdmin
makeIsDataIndexed ''AsterizmAdmin [('AsterizmAdmin,0)]

-- | Setup parameters for @asterizmClient@
data AsterizmSetup = AsterizmSetup
  { acsClientPKH    :: PubKeyHash
  , acsThreadSymbol :: CurrencySymbol
  }

makeLift ''AsterizmSetup
makeIsDataIndexed ''AsterizmSetup [('AsterizmSetup,0)]

newtype ChainId = ChainId Integer
newtype ChainAddress = ChainAddress BuiltinByteString
newtype AsterizmTxId = AsterizmTxId BuiltinByteString
newtype TransferHash = TransferHash BuiltinByteString

data AsterizmTransferMeta = AsterizmTransferMeta
  { atmSrcChainId   :: ChainId
  , atmSrcAddress   :: ChainAddress
  , atmDstChainId   :: ChainId
  , atmDstAddress   :: ChainAddress
  , atmTxId         :: AsterizmTxId
  , atmNotifyFlag   :: Bool
  , atmTransferHash :: TransferHash
  }

makeIsDataIndexed ''ChainId              [('ChainId, 0)]
makeIsDataIndexed ''ChainAddress         [('ChainAddress, 0)]
makeIsDataIndexed ''AsterizmTxId         [('AsterizmTxId, 0)]
makeIsDataIndexed ''TransferHash         [('TransferHash, 0)]
makeIsDataIndexed ''AsterizmTransferMeta [('AsterizmTransferMeta, 0)]

data InitThreadRedeemer
  = Init
  | Clear

makeIsDataIndexed ''InitThreadRedeemer
  [ ('Init, 0)
  , ('Clear, 1)
  ]

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

-- | Asterizm administrator can consume a UTxO with a thread token only if returned to the
-- script with its attached datum intact, or if the thread token is burned.
{-# INLINABLE mkAsterizmInit #-}
mkAsterizmInit :: AsterizmAdmin -> CurrencySymbol -> ScriptContext -> Bool
mkAsterizmInit admin threadSymbol ctx
  | Just TxInInfo { txInInfoResolved  } <- findOwnInput ctx
  , [(tn, m)] <- withCurrencySymbol threadSymbol (txOutValue txInInfoResolved) [] toList
  , m > 0
  , OutputDatum d <- txOutDatum txInInfoResolved
  = signedByAdmin && (datumContinued tn d || threadTokenBurned tn)
  | otherwise = signedByAdmin
 where
  info :: TxInfo
  info = scriptContextTxInfo ctx

  datumContinued :: TokenName -> Datum -> Bool
  datumContinued tn oldDat
    | [TxOut { txOutValue = contValue, txOutDatum = contDatum }] <- getContinuingOutputs ctx
    , valueOf contValue threadSymbol tn == 1
    , OutputDatum newDat <- contDatum
    = newDat == oldDat
    | otherwise = False

  threadTokenBurned ::TokenName -> Bool
  threadTokenBurned tn = valueOf (mintValueBurned $ txInfoMint info) threadSymbol tn == 1
  
  signedByAdmin = txSignedBy info (aaAsterizmPKH admin)

{-# INLINABLE untypedAsterizmInit #-}
untypedAsterizmInit :: AsterizmAdmin -> CurrencySymbol -> BuiltinData -> BuiltinUnit
untypedAsterizmInit admin threadSymbol ctx' = check $ mkAsterizmInit admin threadSymbol ctx
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData ctx'

-- | Mint or burn thread token for @asterizmInit@. Minting requires well structured datum
-- (payload for Asterizm's Relay Contract) and sending the transfer fee.
{-# INLINABLE mkInitThreadPolicy #-}
mkInitThreadPolicy :: AsterizmAdmin -> AsterizmSetup -> InitThreadRedeemer -> ScriptContext -> Bool
mkInitThreadPolicy admin setup redeemer ctx =
  case redeemer of
    Init  -> signedByClient && validMintAndDatum && payedTransferFee
    Clear -> signedByAdmin && validBurn
  where
    info :: TxInfo
    info   = scriptContextTxInfo ctx

    initOut :: TxOut
    initOut = head $ txInfoOutputs info

    minted :: [(TokenName, Integer)]
    minted = case lookup (ownCurrencySymbol ctx) (mintValueToMap $ txInfoMint info) of
      Just ownTokens -> toList ownTokens
      _ -> traceError "NM"

    signedByClient = txSignedBy info (acsClientPKH setup)
    signedByAdmin  = txSignedBy info (aaAsterizmPKH admin)

    validMintAndDatum
      | [(TokenName tn, 1)] <- minted
      , OutputDatum (Datum d) <- txOutDatum initOut
      , Just meta <- fromBuiltinData @AsterizmTransferMeta d
      , let AsterizmTxId tid = atmTxId meta
      = tid == tn
      | otherwise = False

    validBurn = case minted of
      [(_, m)] -> m < 0
      _        -> False

    payedTransferFee = lovelaceValueOf (txOutValue initOut) >= aaAsterizmFee admin

{-# INLINABLE untypedInitThreadPolicy #-}
untypedInitThreadPolicy :: AsterizmAdmin -> AsterizmSetup -> BuiltinData -> BuiltinUnit
untypedInitThreadPolicy admin setup ctx' = check $ mkInitThreadPolicy admin setup redeemer ctx
 where
  ctx :: ScriptContext
  ctx = unsafeFromBuiltinData ctx'

  redeemer :: InitThreadRedeemer
  redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx

asterizmRelayerCompiled :: RelayerPKH -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmRelayerCompiled pkh =
    $$(compile [|| untypedAsterizmRelayer ||])
    `unsafeApplyCode` liftCodeDef pkh

asterizmClientCompiled :: AsterizmSetup -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmClientCompiled setup =
    $$(compile [|| untypedAsterizmClient ||])
    `unsafeApplyCode` liftCodeDef setup

asterizmInitCompiled :: AsterizmAdmin -> CurrencySymbol -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmInitCompiled admin threadSymbol =
    $$(compile [|| untypedAsterizmInit ||])
    `unsafeApplyCode` liftCodeDef admin
    `unsafeApplyCode` liftCodeDef threadSymbol

asterizmInitThreadPolicy :: AsterizmAdmin -> AsterizmSetup -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmInitThreadPolicy admin setup =
    $$(compile [|| untypedInitThreadPolicy ||])
    `unsafeApplyCode` liftCodeDef admin
    `unsafeApplyCode` liftCodeDef setup
