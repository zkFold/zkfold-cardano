{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.RollupSimple (
  rollupSimple,
  rollupSimpleStake,
  module ZkFold.Cardano.UPLC.RollupSimple.Types,
) where

import Data.Function ((&))
import PlutusLedgerApi.V1 (valueOf)
import PlutusLedgerApi.V3
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Prelude hiding (toList)
import ZkFold.Cardano.OnChain.BLS12_381 (toF)
import ZkFold.Cardano.OnChain.Plonkup (PlonkupPlutus)
import ZkFold.Cardano.UPLC.RollupSimple.Types (
  BridgeUtxoInfo (..),
  BridgeUtxoStatus (..),
  RollupConfiguration (..),
  RollupSimpleRed (..),
  RollupState (..),
 )
import ZkFold.Cardano.UPLC.RollupSimple.Utils
import ZkFold.Protocol.NonInteractiveProof (NonInteractiveProof (..))

{-# INLINEABLE rollupSimple #-}
rollupSimple ::
  -- | Script hash of the stake validator.
  BuiltinData ->
  -- | Script context.
  BuiltinData ->
  BuiltinUnit
rollupSimple (unsafeFromBuiltinData -> sh :: ScriptHash) scData =
  check
    $ AssocMap.member (toBuiltinData $ ScriptCredential sh) txInfoWrdl
    && trySpend
    == 1 -- Disallowing any other use-case for now to be on safe-side.
 where
  txInfoL = BI.unsafeDataAsConstr scData & BI.snd
  txInfo = txInfoL & BI.head & BI.unsafeDataAsConstr & BI.snd
  txInfoWrdl :: Map BuiltinData BuiltinData =
    txInfo
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.tail
      & BI.head
      & unsafeFromBuiltinData
  redL = txInfoL & BI.tail
  scriptInfo = redL & BI.tail & BI.head & BI.unsafeDataAsConstr
  trySpend = BI.fst scriptInfo

{-# INLINEABLE rollupSimpleStake #-}
rollupSimpleStake ::
  -- | Rollup configuration.
  BuiltinData ->
  -- | Script context.
  BuiltinData ->
  BuiltinUnit
rollupSimpleStake (unsafeFromBuiltinData -> RollupConfiguration {..}) scData =
  check
    $ if scriptInfoIx == 2
      then
        let
          (RollupSimpleRed {..}) = redL & BI.head & unsafeFromBuiltinData

          goInputs remInputs availableBridgeValAcc bridgeInInitialAcc mownInput =
            case remInputs of
              [] -> (availableBridgeValAcc, bridgeInInitialAcc, mownInput)
              (i' : is) ->
                let i = txInInfoResolved i'
                 in -- Input is relevant.
                    if txOutAddress i == rsrAddress
                      then
                        -- Whether it is state input or an input given to satisfy bridge-out requirement.
                        if valueOf (txOutValue i) rcNftCurrencySymbol rcNftTokenName == 1
                          then
                            goInputs is availableBridgeValAcc bridgeInInitialAcc (Just i')
                          else case txOutDatum i of
                            OutputDatum (getDatum -> odatum) ->
                              case fromBuiltinData odatum of
                                Just (BridgeInInitial l2Addr) ->
                                  goInputs is availableBridgeValAcc ((l2Addr : toSymbolicValue' (txOutValue i)) <> bridgeInInitialAcc) mownInput
                                _ ->
                                  goInputs is (availableBridgeValAcc <> txOutValue i) bridgeInInitialAcc mownInput
                            _ ->
                              goInputs is (availableBridgeValAcc <> txOutValue i) bridgeInInitialAcc mownInput
                      else goInputs is availableBridgeValAcc bridgeInInitialAcc mownInput

          (availableBridgeVal, bridgeInInitialList, Just ownInputInfo) = goInputs txInfoInputs mempty [] Nothing
          ownInputOutput = txInInfoResolved ownInputInfo
          OutputDatum (unsafeFromBuiltinData . getDatum -> (oldState :: RollupState)) = txOutDatum ownInputOutput
          ownInputRef = txInInfoOutRef ownInputInfo

          goOutputs remOutputs bridgeOutReqValAcc bridgeLeftoverValAcc bridgeOutListAcc bridgeInListAcc mcontinuingOutput =
            case remOutputs of
              [] -> (bridgeOutReqValAcc, bridgeLeftoverValAcc, bridgeOutListAcc, bridgeInListAcc, mcontinuingOutput)
              (o : os) ->
                case txOutDatum o of
                  NoOutputDatum -> goOutputs os bridgeOutReqValAcc bridgeLeftoverValAcc bridgeOutListAcc bridgeInListAcc mcontinuingOutput
                  OutputDatumHash _ -> goOutputs os bridgeOutReqValAcc bridgeLeftoverValAcc bridgeOutListAcc bridgeInListAcc mcontinuingOutput
                  OutputDatum (getDatum -> odatum) ->
                    if txOutAddress o == rsrAddress
                      then
                        if valueOf (txOutValue o) rcNftCurrencySymbol rcNftTokenName == 1
                          then
                            goOutputs os bridgeOutReqValAcc bridgeLeftoverValAcc bridgeOutListAcc bridgeInListAcc (Just o)
                          else
                            let odatum' :: BridgeUtxoInfo = unsafeFromBuiltinData odatum
                             in if buiORef odatum' == ownInputRef
                                  then case buiStatus odatum' of
                                    BridgeIn layer2Address ->
                                      goOutputs os bridgeOutReqValAcc bridgeLeftoverValAcc bridgeOutListAcc ((layer2Address : toSymbolicValue' (txOutValue o)) <> bridgeInListAcc) mcontinuingOutput
                                    BridgeBalance ->
                                      goOutputs os bridgeOutReqValAcc (txOutValue o <> bridgeLeftoverValAcc) bridgeOutListAcc bridgeInListAcc mcontinuingOutput
                                    BridgeInInitial _ ->
                                      traceError "rollupSimpleStake: bridge-in-initial output cannot be part of state update transaction"
                                    BridgeOut -> traceError "rollupSimpleStake: bridge-out output cannot be to the rollup validator"
                                  else
                                    traceError "rollupSimpleStake: output to rollup validator must be either a bridge-in, bridge-balance or state UTxO"
                      else
                        if odatum == toBuiltinData (BridgeUtxoInfo ownInputRef BridgeOut)
                          then
                            goOutputs os (bridgeOutReqValAcc <> txOutValue o) bridgeLeftoverValAcc ((byteStringToInteger' (addressToBS (txOutAddress o)) : toSymbolicValue' (txOutValue o)) <> bridgeOutListAcc) bridgeInListAcc mcontinuingOutput
                          else goOutputs os bridgeOutReqValAcc bridgeLeftoverValAcc bridgeOutListAcc bridgeInListAcc mcontinuingOutput
          (bridgeOutReqVal, bridgeLeftoverVal, bridgeOutList, bridgeInList, Just continuingOutput) = goOutputs txInfoOutputs mempty mempty mempty mempty Nothing
          OutputDatum (unsafeFromBuiltinData . getDatum -> (newState :: RollupState)) = txOutDatum continuingOutput
         in
          -- Remaining funds are securely returned to the validator.
          traceIfFalse
            "rollupSimpleStake: availableBridgeVal mismatch"
            ( availableBridgeVal
                == (bridgeOutReqVal <> bridgeLeftoverVal)
            )
            && traceIfFalse
              "rollupSimpleStake: proof verification failed"
              ( verify @PlonkupPlutus
                  rcSetupBytes
                  ( toF
                      <$> [previousStateHash oldState, utxoTreeRoot oldState, chainLength oldState, bridgeInCommitment oldState, bridgeOutCommitment oldState, previousStateHash newState, utxoTreeRoot newState, chainLength newState, bridgeInCommitment newState, bridgeOutCommitment newState, 1]
                      <> (bridgeInList <> fillWithZeros3WithAdd (rcMaxBridgeIn - quot (length bridgeInList)) rcMaxOutputAssets 3 [])
                      <> (bridgeOutList <> fillWithZeros3WithAdd (rcMaxBridgeOut - quot (length bridgeOutList)) rcMaxOutputAssets 3 [])
                  )
                  rsrProofBytes
              )
            && checkPrefix bridgeInInitialList bridgeInList
      else
        let
          txCertIx = BI.snd scriptInfo & BI.tail & BI.head & BI.unsafeDataAsConstr & BI.fst
         in
          scriptInfoIx
            == 3
            && txCertIx
            == 0 -- Allow for registering of stake validator.
 where
  quot = (`quotient` (1 + 3 * rcMaxOutputAssets))
  txInfoL = BI.unsafeDataAsConstr scData & BI.snd
  txInfo = txInfoL & BI.head & BI.unsafeDataAsConstr & BI.snd
  txInfoInputs = BI.head txInfo & unsafeFromBuiltinData @[TxInInfo]
  txInfoOutputs = txInfo & BI.tail & BI.tail & BI.head & unsafeFromBuiltinData @[TxOut]
  redL = txInfoL & BI.tail
  scriptInfo = redL & BI.tail & BI.head & BI.unsafeDataAsConstr
  scriptInfoIx = BI.fst scriptInfo
  toSymbolicValue' = toSymbolicValue rcMaxOutputAssets
  -- Check if `p` is a prefix of `l`.
  checkPrefix p l =
    case p of
      [] -> True
      (x : xs) ->
        case l of
          [] -> False
          (y : ys) -> (x == y) && checkPrefix xs ys
