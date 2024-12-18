{-# LANGUAGE TemplateHaskell #-}

module ZkFold.Cardano.UPLC.Common where

import           PlutusLedgerApi.V1.Value (flattenValue)
import           PlutusLedgerApi.V3
-- import           PlutusTx                 (CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude         (Bool (..), BuiltinUnit, Integer, any, check, ($), (&&), (==))


---------------------------- :nftPolicy: ----------------------------

{-# INLINABLE nftPolicy #-}
nftPolicy :: TxOutRef -> ScriptContext -> Bool
nftPolicy oref ctx = hasUTxO && checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, _, amt)] -> amt == 1
        _             -> False

{-# INLINABLE untypedNftPolicy #-}
untypedNftPolicy :: TxOutRef -> BuiltinData -> BuiltinUnit
untypedNftPolicy oref ctx' =
  let
    ctx = unsafeFromBuiltinData ctx'
  in
    check $ nftPolicy oref ctx

--------------------------- :parkingSpot: ---------------------------

{-# INLINABLE untypedParkingSpot #-}
untypedParkingSpot :: Integer -> BuiltinData -> BuiltinUnit
untypedParkingSpot _ _ = check True
