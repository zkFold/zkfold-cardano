{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.UPLC.Rollup where

import           GHC.ByteOrder                            (ByteOrder (..))
import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V3
import           PlutusLedgerApi.V3.Contexts              (findOwnInput)
import           PlutusTx                                 (makeIsDataIndexed, makeLift)
import           PlutusTx.AssocMap                        (lookup, toList)
import           PlutusTx.Builtins                        (mkI, unsafeDataAsI)
import           PlutusTx.Prelude                         hiding (toList, (*), (+))
import           Prelude                                  (Show)

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain.BLS12_381.F       (toF)
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes, SetupBytes)
import           ZkFold.Cardano.OnChain.Utils             (dataToBlake)

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

-- | Plutus script for verifying a rollup state transition.
{-# INLINABLE rollup #-}
rollup :: RollupSetup -> RollupRedeemer -> ScriptContext -> Bool
rollup (RollupSetup ledgerRules dataCurrency threadValue feeAddress) (UpdateRollup proof update) ctx =
  let
    -- Get the current rollup output
    out = txInInfoResolved $ case findOwnInput ctx of
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
      mapMaybe (lookup dataCurrency) $
      map (getValue . txOutValue . txInInfoResolved) (txInfoReferenceInputs $ scriptContextTxInfo ctx)

    -- Get the next rollup output
    out'   = head $ txInfoOutputs $ scriptContextTxInfo ctx

    -- Get the fee output
    outFee = head $ tail $ txInfoOutputs $ scriptContextTxInfo ctx
    feeVal = getValue $ txOutValue outFee

    -- Get bridge outputs
    -- If the payment credential of the output coincides with the rollup payment credential, then this output transfers value to the rollup.
    -- Otherwise, it transfers value from the rollup.
    bridgeOutputs =
      filter (\case
        TxOut _ _ (OutputDatumHash _) Nothing -> True
        _                                     -> False)
      $ tail $ tail $ tail $ txInfoOutputs $ scriptContextTxInfo ctx

    -- Compute the next state
    state' = byteStringToInteger BigEndian $ dataToBlake (toF state, update, bridgeOutputs, feeVal)

    -- Get thread currency symbol
  in
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

{-# INLINABLE untypedRollup #-}
untypedRollup :: RollupSetup -> BuiltinData -> BuiltinUnit
untypedRollup computation ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ rollup computation redeemer ctx

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
