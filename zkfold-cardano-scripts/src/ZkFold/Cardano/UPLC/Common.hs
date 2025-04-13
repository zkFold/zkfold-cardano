{-# LANGUAGE TemplateHaskell #-}

module ZkFold.Cardano.UPLC.Common where

import           PlutusLedgerApi.V1.Value (flattenValue)
import           PlutusLedgerApi.V3       (BuiltinData, ScriptContext (..), TxInInfo (..), TxInfo (..), TxOutRef,
                                           UnsafeFromData (..), mintValueBurned, mintValueMinted)
import           PlutusTx                 (CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude         (Bool (..), BuiltinUnit, Integer, Monoid (mempty), any, check, ($), (&&),
                                           (==))

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
    checkMintedAmount = mintValueBurned txInfoMint' == mempty && case flattenValue (mintValueMinted txInfoMint') of
        [(_, _, amt)] -> amt == 1
        _             -> False
      where
        txInfoMint' = txInfoMint info

{-# INLINABLE untypedNftPolicy #-}
untypedNftPolicy :: TxOutRef -> BuiltinData -> BuiltinUnit
untypedNftPolicy oref ctx' =
  let
    ctx = unsafeFromBuiltinData ctx'
  in
    check $ nftPolicy oref ctx

nftPolicyCompiled :: TxOutRef -> CompiledCode (BuiltinData -> BuiltinUnit)
nftPolicyCompiled oref =
  $$(compile [|| untypedNftPolicy ||])
  `unsafeApplyCode` liftCodeDef oref

--------------------------- :parkingSpot: ---------------------------

{-# INLINABLE untypedParkingSpot #-}
untypedParkingSpot :: Integer -> BuiltinData -> BuiltinUnit
untypedParkingSpot _ _ = check True

parkingSpotCompiled :: Integer -> CompiledCode (BuiltinData -> BuiltinUnit)
parkingSpotCompiled tag =
    $$(compile [|| untypedParkingSpot ||])
    `unsafeApplyCode` liftCodeDef tag
