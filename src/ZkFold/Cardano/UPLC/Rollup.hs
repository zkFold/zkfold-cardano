{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.UPLC.Rollup where

import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V3
import           PlutusLedgerApi.V3.Contexts              (findOwnInput)
import           PlutusTx                                 (makeIsDataIndexed, makeLift)
import           PlutusTx.AssocMap                        (toList)
import           PlutusTx.Builtins                        (unsafeDataAsI)
import           PlutusTx.Prelude                         hiding ((*), (+), toList)
import           Prelude                                  (Show)

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain.BLS12_381         (toInput)
import           ZkFold.Cardano.OnChain.Plonk             (PlonkPlutus)
import           ZkFold.Cardano.OnChain.Plonk.Data        (ProofBytes, SetupBytes)
import           ZkFold.Cardano.OnChain.Utils             (dataToBlake)

data RollupSetup = RollupSetup
  { rsLedgerRules  :: SetupBytes
  , rsFeeAddress   :: Address
  , rsDataCurrency :: CurrencySymbol
  } deriving stock (Show, Generic)

makeLift ''RollupSetup
makeIsDataIndexed ''RollupSetup [('RollupSetup,0)]

data RollupRedeemer =
      UpdateRollup ProofBytes [BuiltinByteString]
    -- ^ Update the rollup state using the proof.
    | CombineValue
    -- ^ Combine the non-ada values locked in the rollup.
    | AdjustStake
    -- ^ Adjust the stake of the ada value locked in the rollup.
    | UpgradeScript
    -- ^ Update the script of the rollup to a new version.
  deriving stock (Show, Generic)

makeIsDataIndexed ''RollupRedeemer [('UpdateRollup,0),('CombineValue,1),('AdjustStake,2),('UpgradeScript,3)]

-- | Plutus script for verifying a rollup state transition.
{-# INLINABLE rollup #-}
rollup :: RollupSetup -> RollupRedeemer -> ScriptContext -> Bool
rollup (RollupSetup ledgerRules feeAddress dataCurrency) (UpdateRollup proof update) ctx =
  let
    -- Get the current rollup output
    out = txInInfoResolved $ case findOwnInput ctx of
      Just j -> j
      _      -> traceError "rollup: no input"

    -- Get the address and state of the rollup
    (addr, state) = case out of
      TxOut addr' _ (OutputDatum (Datum s)) _ -> (addr', unsafeDataAsI s)
      _ -> traceError "rollup: invalid redeemer"

    -- Get state updates as token names of the data currency
    update' =
      map (unTokenName . fst) $
      concatMap (toList . snd) $
      filter (\(k, _) -> k == dataCurrency) $
      -- Every referenced input must have the data currency as the second currency (the first one is ada).
      map (head . tail . toList . getValue . txOutValue . txInInfoResolved)
      (txInfoReferenceInputs $ scriptContextTxInfo ctx)

    -- Get the next rollup output
    out'   = head $ txInfoOutputs $ scriptContextTxInfo ctx

    -- Get the fee output
    outFee = head $ tail $ txInfoOutputs $ scriptContextTxInfo ctx
    feeVal = getValue $ txOutValue outFee

    -- Get bridge outputs
    -- If the payment credential of the output coincides with the rollup payment credential, then this output transfers value to the rollup.
    -- Otherwise, it transfers value from the rollup.
    bridgeOutputs =
      filter (\(TxOut _ _ (OutputDatumHash _) Nothing) -> True) $
      tail $ tail $ tail $ txInfoOutputs $ scriptContextTxInfo ctx

    -- Compute the next state
    state' = toInput $ dataToBlake (state, update, bridgeOutputs, feeVal)
  in
    -- Verify the transition from the current state to the next state
    verify @PlonkPlutus @HaskellCore ledgerRules state' proof

    -- Compare the state updates
    && sort update' == sort update

    -- Check the next rollup output
    && case out' of
      TxOut a _ (OutputDatum (Datum s)) Nothing -> addr == a && toBuiltinData state' == s
      _ -> False

    -- Check the fee output
    && case outFee of
      TxOut addr'' _ NoOutputDatum Nothing -> feeAddress == addr''
      _ -> False
-- TODO: implement other cases
rollup _ _ _ = False

{-# INLINABLE untypedRollup #-}
untypedRollup :: RollupSetup -> BuiltinData -> BuiltinUnit
untypedRollup computation ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ rollup computation redeemer ctx
