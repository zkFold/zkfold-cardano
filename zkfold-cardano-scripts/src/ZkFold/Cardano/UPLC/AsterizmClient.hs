{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.AsterizmClient where

import           Data.Coerce                 (coerce)
import           Data.Maybe                  (fromJust)
import           PlutusLedgerApi.V1          (currencySymbolValueOf, flattenValue, symbols)
import           PlutusLedgerApi.V1.Value    (withCurrencySymbol)
import           PlutusLedgerApi.V3          as V3
import           PlutusLedgerApi.V3.Contexts (ownCurrencySymbol, txSignedBy)
import           PlutusTx                    (CompiledCode, compile, liftCodeDef, makeIsDataIndexed, makeLift, unsafeApplyCode)
import           PlutusTx.AssocMap           (keys)
import           PlutusTx.Foldable           (foldMap)
import           PlutusTx.Prelude            (Bool (..), BuiltinUnit, Integer, Ord (..), blake2b_256, check, elem, find, head,
                                              ($), (<$>), (&&), (.), (==), (/=), (||))
import           PlutusTx.Trace              (traceError)


-- | Setup parameters for @asterizmClient@
data AsterizmClientSetup = AsterizmClientSetup
  { acsClientPKH    :: PubKeyHash
  , acsThreadSymbol :: CurrencySymbol
  }

makeLift ''AsterizmClientSetup
makeIsDataIndexed ''AsterizmClientSetup [('AsterizmClientSetup,0)]

-- | Plutus script (minting policy) for posting actual messages on-chain.
{-# INLINABLE untypedAsterizmClient #-}
untypedAsterizmClient :: AsterizmClientSetup -> BuiltinData -> BuiltinUnit
untypedAsterizmClient AsterizmClientSetup{..} ctx' = check $
    conditionBurning || conditionMinting && conditionSigned && conditionVerifying
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData ctx'

    info :: TxInfo
    info = scriptContextTxInfo ctx

    refInputs :: [TxOut]
    refInputs = txInInfoResolved <$> txInfoReferenceInputs info

    threadInput :: TxOut
    threadInput = fromJust $
      find (\i -> currencySymbolValueOf (txOutValue i) acsThreadSymbol == 1) refInputs

    valueReferenced :: Value
    valueReferenced = foldMap txOutValue refInputs

    minted :: [(CurrencySymbol, TokenName, Integer)]
    minted = flattenValue . coerce . mintValueToMap $ txInfoMint info

    (cs, tn, amt) = case minted of
      [x] -> x
      _   -> traceError "Expected exactly one minting action"

    message :: BuiltinByteString
    message = unsafeFromBuiltinData . getRedeemer $ scriptContextRedeemer ctx

    relayers :: [CurrencySymbol] 
    relayers = case txOutDatum threadInput of
                 OutputDatum d -> unsafeFromBuiltinData $ getDatum d
                 _             -> traceError "Missing registry"

    relayerCS :: CurrencySymbol
    relayerCS = fromJust . find (\s -> s /= adaSymbol && s `elem` relayers) $ symbols valueReferenced

    tokenName = TokenName $ blake2b_256 message

    conditionMinting = cs == ownCurrencySymbol ctx && tn == tokenName

    conditionVerifying = withCurrencySymbol relayerCS valueReferenced False $ \tm ->
      head (keys tm) == tokenName

    conditionSigned = txSignedBy info acsClientPKH

    conditionBurning = amt < 0

asterizmClientCompiled :: AsterizmClientSetup -> CompiledCode (BuiltinData -> BuiltinUnit)
asterizmClientCompiled setup =
    $$(compile [|| untypedAsterizmClient ||])
    `unsafeApplyCode` liftCodeDef setup
