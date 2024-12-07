{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}


module ZkFold.Cardano.UPLC.Rollup where

import           GHC.ByteOrder                            (ByteOrder (..))
import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V3
import           PlutusTx                                 (makeIsDataIndexed, makeLift)
import           PlutusTx.AssocMap                        (lookup, toList)
import           PlutusTx.Builtins                        (mkI, serialiseData, unsafeDataAsI)
import qualified PlutusTx.Builtins.Internal               as BI
import           PlutusTx.Prelude                         hiding (toList, (*), (+))
import           Prelude                                  (Show)

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain.BLS12_381.F       (toF)
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes, SetupBytes)
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)

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

findOwnInput' :: [TxInInfo] -> TxOutRef -> Maybe TxInInfo
findOwnInput' txInfoInputs txOutRef = find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
{-# INLINABLE findOwnInput' #-}

filter' :: (BuiltinData -> Bool) -> BI.BuiltinList BuiltinData -> BI.BuiltinList BuiltinData
filter' pred' xs = BI.ifThenElse (BI.null xs) (BI.mkNilData BI.unitval) $
  let
    x'  = BI.head xs
    xs' = BI.tail xs
  in if pred' x' then BI.mkCons x' (filter' pred' xs') else filter' pred' xs'
{-# INLINABLE filter' #-}

-- | Plutus script for verifying a rollup state transition.
{-# INLINABLE untypedRollup #-}
untypedRollup :: RollupSetup -> BuiltinData -> BuiltinUnit
untypedRollup (RollupSetup ledgerRules dataCurrency threadValue feeAddress) ctx' =
  let
    -- Extracting transaction builtin fields
    scriptContextTxInfo' = BI.snd $ BI.unsafeDataAsConstr ctx'
    infBeforeReInputs    = BI.tail $ BI.snd $ BI.unsafeDataAsConstr $ BI.head scriptContextTxInfo'

    refs   = BI.head infBeforeReInputs
    outs   = BI.unsafeDataAsList $ BI.head $ BI.tail infBeforeReInputs

    scriptContextRedeemer' = BI.tail scriptContextTxInfo'
    scriptInfo  = BI.unsafeDataAsConstr $ BI.head $ BI.tail scriptContextRedeemer'
    trySpend    = BI.fst scriptInfo
    spendScript = BI.snd scriptInfo

    spendRef = unsafeFromBuiltinData @TxOutRef $ BI.head spendScript
    refs'    = unsafeFromBuiltinData @[TxInInfo] refs

    -- Get the current rollup output
    out = txInInfoResolved $ case findOwnInput' refs' spendRef of
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
      mapMaybe (lookup dataCurrency . getValue . txOutValue . txInInfoResolved) refs'

    -- Get the next rollup output
    out'   = unsafeFromBuiltinData @TxOut $ BI.head outs

    -- Get the fee output
    outFeeB = BI.head $ BI.tail outs
    addr''                  = BI.snd $ BI.unsafeDataAsConstr outFeeB
    txOutDatum''            = BI.tail $ BI.tail addr''
    txOutReferenceScript''  = BI.tail txOutDatum''
    addr'         = unsafeFromBuiltinData @Address $ BI.head addr''
    noOutputDatum = BI.fst $ BI.unsafeDataAsConstr $ BI.head txOutDatum''
    scriptHashN   = BI.fst $ BI.unsafeDataAsConstr $ BI.head txOutReferenceScript''

    -- Get bridge outputs
    -- If the payment credential of the output coincides with the rollup payment credential, then this output transfers value to the rollup.
    -- Otherwise, it transfers value from the rollup.
    bridgeOutputsB = BI.mkList $
      filter' (\x -> 
        let x' = BI.snd $ BI.unsafeDataAsConstr x
            txOutDatum' = BI.tail $ BI.tail x' 
            txOutReferenceScript' = BI.tail txOutDatum'
            outputDatumHash = BI.fst $ BI.unsafeDataAsConstr $ BI.head txOutDatum'
            scriptHashNothing = BI.fst $ BI.unsafeDataAsConstr $ BI.head txOutReferenceScript'
        in outputDatumHash == 1 && scriptHashNothing == 0)
      $ BI.tail $ BI.tail $ BI.tail outs

    -- Extract redeemer from ScriptContext
    upateRollup = BI.snd $ BI.unsafeDataAsConstr $ BI.head scriptContextRedeemer'
    proof = unsafeFromBuiltinData @ProofBytes $ BI.head upateRollup

    stateB         = toBuiltinData $ toF state
    updateB        = BI.head $ BI.tail upateRollup
    feeValB        = BI.head $ BI.tail $ BI.snd $ BI.unsafeDataAsConstr outFeeB

    mkTuple4 a b c d =
        BI.mkList $
          BI.mkCons a $
            BI.mkCons b $
              BI.mkCons c $
                BI.mkCons d $
                  BI.mkNilData BI.unitval

    update = unsafeFromBuiltinData @[BuiltinByteString] updateB

    -- Compute the next state
    state' = byteStringToInteger BigEndian $ blake2b_224 . serialiseData $ mkTuple4 stateB updateB bridgeOutputsB feeValB

    -- Get thread currency symbol
  in check $
    trySpend == 1 && -- we must be SpendingScript for findOwnInput
    -- Verify the transition from the current state to the next state
    verify @PlonkupPlutus @HaskellCore ledgerRules (toF state') proof

    -- Compare the state updates
    -- Note: we want to have the full control over the order of data updates. That is why we pass `update` in the redeemer.
    && sort update' == sort update

    -- Check the current rollup output
    && val == threadValue

    -- Check the next rollup output
    && out' == TxOut addr threadValue (OutputDatum (Datum $ mkI state')) Nothing

    -- Check the fee output
    && noOutputDatum == 0 && scriptHashN == 0 && feeAddress == addr'

-- rollup (RollupSetup _ _ threadValue _) ForwardValidation ctx =
--   let
--     out = head $ txInfoOutputs $ scriptContextTxInfo ctx
--   in
--     txOutValue out == threadValue
-- -- TODO: implement other cases
-- rollup _ _ _ = False

{-# INLINABLE parkingSpot #-}
parkingSpot :: Integer -> ScriptContext -> Bool
parkingSpot _ _ = True

{-# INLINABLE untypedParkingSpot #-}
untypedParkingSpot :: Integer -> BuiltinData -> BuiltinUnit
untypedParkingSpot tag ctx' =
  let
    ctx = unsafeFromBuiltinData ctx'
  in
    check $ parkingSpot tag ctx
