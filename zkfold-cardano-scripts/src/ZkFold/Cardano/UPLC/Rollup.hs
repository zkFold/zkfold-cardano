{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.Rollup where

import           GHC.ByteOrder                       (ByteOrder (..))
import           GHC.Generics                        (Generic)
import           PlutusLedgerApi.V3
import           PlutusTx                            (CompiledCode, compile, liftCodeDef, makeIsDataIndexed, makeLift,
                                                      unsafeApplyCode)
import           PlutusTx.AssocMap                   (lookup, toList)
import           PlutusTx.Builtins                   (mkI, unsafeDataAsI)
import qualified PlutusTx.Builtins.Internal          as BI
import           PlutusTx.Prelude                    hiding (toList, (*), (+))
import           Prelude                             (Show)

import           ZkFold.Cardano.OnChain.BLS12_381.F  (F (..), toF)
import           ZkFold.Cardano.OnChain.Plonkup      (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes, SetupBytes)
import           ZkFold.Cardano.OnChain.Utils        (dataToBlake, findOwnInput')
import           ZkFold.Protocol.NonInteractiveProof (NonInteractiveProof (..))

data RollupSetup = RollupSetup
  { rsLedgerRules  :: SetupBytes
  , rsDataCurrency :: CurrencySymbol
  , rsThreadValue  :: Value
  , rsFeeAddress   :: Address
  } deriving stock (Show, Generic)

makeLift ''RollupSetup
makeIsDataIndexed ''RollupSetup [('RollupSetup,0)]

data RollupRedeemer =
      UpdateRollup ProofBytes [BuiltinByteString]
    -- -- ^ Update the rollup state using the proof.
    -- | ForwardValidation
    -- -- ^ Forwards validation of the rollup state update.
    -- | CombineValue
    -- -- ^ Combine the non-ada values locked in the rollup.
    -- | AdjustStake
    -- -- ^ Adjust the stake of the ada value locked in the rollup.
    -- | UpgradeScript
    -- -- ^ Update the script of the rollup to a new version.
  deriving stock (Show, Generic)

-- makeIsDataIndexed ''RollupRedeemer [('UpdateRollup,0),('ForwardValidation,1),('CombineValue,2),('AdjustStake,3),('UpgradeScript,4)]
makeIsDataIndexed ''RollupRedeemer [('UpdateRollup, 0)]

data RollupInfo = RollupInfo
    { riParkingTag :: Integer
    , riDataUpdate :: [[BuiltinByteString]]
    , riState      :: F
    , riRedeemer   :: RollupRedeemer
    } deriving stock (Show, Generic)

makeIsDataIndexed ''RollupInfo [('RollupInfo,0)]

-- | Plutus script for verifying a rollup state transition.
{-# INLINABLE untypedRollup #-}
untypedRollup :: RollupSetup -> BuiltinData -> BuiltinUnit
untypedRollup (RollupSetup ledgerRules dataCurrency threadValue feeAddress) ctx' =
  let
    -- Extracting ScriptContext general fields
    scriptContextTxInfo   = BI.snd $ BI.unsafeDataAsConstr ctx'
    scriptContextRedeemer = BI.tail scriptContextTxInfo

    -- Extracting transaction builtin fields
    info              = BI.head scriptContextTxInfo
    infoFields        = BI.snd $ BI.unsafeDataAsConstr info
    infBeforeReInputs = BI.tail infoFields

    -- Extracting transaction data
    ins  = BI.head infoFields
    refs = BI.head infBeforeReInputs

    outs = BI.unsafeDataAsList $ BI.head $ BI.tail infBeforeReInputs
    txInfoInputs          = unsafeFromBuiltinData @[TxInInfo] ins
    txInfoReferenceInputs = unsafeFromBuiltinData @[TxInInfo] refs

    -- Extracting ScriptInfo
    scriptInfo = BI.unsafeDataAsConstr $ BI.head $ BI.tail scriptContextRedeemer
    trySpend   = BI.fst scriptInfo
    spendRef   = unsafeFromBuiltinData @TxOutRef $ BI.head $ BI.snd scriptInfo

    -- Extract Redeemer from ScriptContext
    (UpdateRollup proof update) = unsafeFromBuiltinData @RollupRedeemer $ BI.head scriptContextRedeemer

    -- Get the current rollup output
    out = txInInfoResolved $ case findOwnInput' txInfoInputs spendRef of
      Just j -> j
      _      -> traceError "rollup: no input"

    -- Get the address and state of the rollup
    (addr, val, state) = case out of
      TxOut a v (OutputDatum (Datum s)) _ -> (a, v, unsafeDataAsI s)
      _                                   -> traceError "rollup: invalid redeemer"

    -- Get state updates as token names of the data currency
    update' =
      map (unTokenName . fst) $
      concatMap toList $
      mapMaybe (lookup dataCurrency . getValue . txOutValue . txInInfoResolved) txInfoReferenceInputs

    -- Get the next rollup output
    out' = unsafeFromBuiltinData @TxOut $ BI.head outs

    -- Get the fee output
    outFee  = unsafeFromBuiltinData @TxOut $ BI.head $ BI.tail outs
    feeVal  = getValue $ txOutValue outFee

    -- Get bridge outputs
    -- If the payment credential of the output coincides with the rollup payment credential, then this output transfers value to the rollup.
    -- Otherwise, it transfers value from the rollup.
    bridgeOutputs =
      filter (\case
        TxOut _ _ (OutputDatumHash _) Nothing -> True
        _                                     -> False)
      $ unsafeFromBuiltinData @[TxOut] $ BI.mkList $ BI.tail $ BI.tail outs

    -- Compute the next state
    state' = byteStringToInteger BigEndian $ dataToBlake (toF state, update, bridgeOutputs, feeVal)

  in check $
    -- Must be SpendingScript for findOwnInput
    trySpend == 1

    -- Verify the transition from the current state to the next state
    && verify @PlonkupPlutus ledgerRules [toF state'] proof

    -- Compare the state updates
    -- Note: we want to have the full control over the order of data updates. That is why we pass `update` in the redeemer.
    && sort update' == sort update

    -- Check the current rollup output
    && val == threadValue

    -- Check the next rollup output
    && out' == TxOut addr threadValue (OutputDatum (Datum $ mkI state')) Nothing

    -- Check the fee output
    && case outFee of
         TxOut addr'' _ NoOutputDatum Nothing -> feeAddress == addr''
         _                                    -> False

-- rollup (RollupSetup _ _ threadValue _) ForwardValidation ctx =
--   let
--     out = head $ txInfoOutputs $ scriptContextTxInfo ctx
--   in
--     txOutValue out == threadValue
-- -- TODO: implement other cases
-- rollup _ _ _ = False

rollupCompiled :: RollupSetup -> CompiledCode (BuiltinData -> BuiltinUnit)
rollupCompiled computation =
    $$(compile [|| untypedRollup ||])
    `unsafeApplyCode` liftCodeDef computation
