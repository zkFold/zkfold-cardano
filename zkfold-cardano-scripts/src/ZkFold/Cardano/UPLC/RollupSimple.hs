{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.RollupSimple (
  rollupSimple,
  module ZkFold.Cardano.UPLC.RollupSimple.Types,
) where
import           Data.Function                          ((&))
import           PlutusLedgerApi.V1                     (valueOf)
import           PlutusLedgerApi.V3
import qualified PlutusTx.Builtins.Internal             as BI
import           PlutusTx.Prelude                       hiding (toList, (*), (+))

import           ZkFold.Cardano.OnChain.BLS12_381       (toF)
import           ZkFold.Cardano.OnChain.Plonkup         (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data    (SetupBytes)
import           ZkFold.Cardano.OnChain.Utils           (findOwnInput')
import           ZkFold.Cardano.UPLC.RollupSimple.Types (RollupSimpleRed, RollupState (..))
import           ZkFold.Protocol.NonInteractiveProof    (NonInteractiveProof (..))

{-# INLINEABLE rollupSimple #-}
rollupSimple ::
  -- | Setup bytes.
  BuiltinData ->
  -- | NFT Currency Symbol.
  BuiltinData ->
  -- | NFT Token Name.
  BuiltinData ->
  -- | Script context.
  BuiltinData ->
  BuiltinUnit
rollupSimple (unsafeFromBuiltinData -> (setupBytes :: SetupBytes)) (unsafeFromBuiltinData -> (nftCurrencySymbol :: CurrencySymbol)) (unsafeFromBuiltinData -> (nftTokenName :: TokenName)) scData = check $
  trySpend == 1
  && valueOf (txOutValue ownInputOutput) nftCurrencySymbol nftTokenName == 1
  && if verify @PlonkupPlutus setupBytes (toF <$> [previousStateHash oldState, utxoTreeRoot oldState, chainLength oldState, bridgeInCommitment oldState, bridgeOutCommitment oldState, previousStateHash newState, utxoTreeRoot newState, chainLength newState, bridgeInCommitment newState, bridgeOutCommitment newState, 1]) proofBytes then True else traceError "Rollup proof verification failed"
  where

    txInfoL = BI.unsafeDataAsConstr scData & BI.snd
    txInfo = txInfoL & BI.head & BI.unsafeDataAsConstr & BI.snd
    txInfoInputs  = BI.head txInfo & unsafeFromBuiltinData @[TxInInfo]
    txInfoOutputs = txInfo & BI.tail & BI.tail & BI.head & unsafeFromBuiltinData @[TxOut]
    redL = txInfoL & BI.tail
    proofBytes = redL & BI.head & unsafeFromBuiltinData
    -- Extracting ScriptInfo
    scriptInfo = redL & BI.tail & BI.head & BI.unsafeDataAsConstr
    trySpend   = BI.fst scriptInfo
    spendFields = scriptInfo & BI.snd
    spendRef   = spendFields & BI.head & unsafeFromBuiltinData @TxOutRef
    Just (unsafeFromBuiltinData . getDatum -> (oldState :: RollupState)) = spendFields & BI.tail & BI.head & unsafeFromBuiltinData @(Maybe Datum)
    Just ownInput = findOwnInput' txInfoInputs spendRef
    ownInputOutput = txInInfoResolved ownInput
    Just continuingOutput = find (\txOut -> txOutAddress txOut == txOutAddress ownInputOutput && valueOf (txOutValue txOut) nftCurrencySymbol nftTokenName == 1) txInfoOutputs
    OutputDatum (unsafeFromBuiltinData . getDatum -> (newState :: RollupState)) = txOutDatum continuingOutput
