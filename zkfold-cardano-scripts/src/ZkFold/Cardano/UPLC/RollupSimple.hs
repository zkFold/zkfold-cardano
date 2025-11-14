{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkFold.Cardano.UPLC.RollupSimple (
  rollupSimple,
) where
import           Data.Function                       ((&))
import           GHC.Generics                        (Generic)
import           PlutusLedgerApi.V1                  (valueOf)
import           PlutusLedgerApi.V3
import           PlutusTx.Blueprint
import qualified PlutusTx.Blueprint.TH
import qualified PlutusTx.Builtins.Internal          as BI
import           PlutusTx.Prelude                    hiding (toList, (*), (+))
import           Prelude                             (Show)

import           ZkFold.Cardano.OnChain.BLS12_381    (toF)
import           ZkFold.Cardano.OnChain.Plonkup      (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes, SetupBytes)
import           ZkFold.Cardano.OnChain.Utils        (findOwnInput')
import           ZkFold.Protocol.NonInteractiveProof (NonInteractiveProof (..))

data RollupState = RollupState
  { previousStateHash   :: Integer
  , utxoTreeRoot        :: Integer
  , chainLength         :: Integer
  , bridgeInCommitment  :: Integer
  , bridgeOutCommitment :: Integer
  }
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''RollupState [('RollupState, 0)]

newtype RollupSimpleRed = RollupSimpleRed ProofBytes
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

PlutusTx.Blueprint.TH.makeIsDataSchemaIndexed ''RollupSimpleRed [('RollupSimpleRed, 0)]

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
  && verify @PlonkupPlutus setupBytes (toF <$> [previousStateHash oldState, utxoTreeRoot oldState, chainLength oldState, bridgeInCommitment oldState, bridgeOutCommitment oldState, previousStateHash newState, utxoTreeRoot newState, chainLength newState, bridgeInCommitment newState, bridgeOutCommitment newState, 1]) proofBytes
  where

    txInfoL = BI.unsafeDataAsConstr scData & BI.snd
    txInfo = txInfoL & BI.head & BI.unsafeDataAsConstr & BI.snd
    txInfoInputs  = BI.head txInfo & unsafeFromBuiltinData @[TxInInfo]
    txInfoOutputs = txInfo & BI.tail & BI.tail & BI.head & unsafeFromBuiltinData @[TxOut]
    redL = txInfoL & BI.tail
    RollupSimpleRed proofBytes = redL & BI.head & unsafeFromBuiltinData
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
