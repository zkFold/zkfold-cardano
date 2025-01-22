{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ZkFold.Cardano.UPLC.PlonkVerifierTx where

import           PlutusLedgerApi.V3                       (ToData (..), TxInInfo (..), TxOut (..))
import           PlutusTx                                 (unsafeFromBuiltinData)
import qualified PlutusTx.Builtins.Internal               as BI
import           PlutusTx.Prelude                         (BuiltinData, BuiltinUnit, blake2b_224, check,
                                                           filter, isNothing, traceError, ($), (.))

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain.BLS12_381.F       (toInput)
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes, SetupBytes)

-- | Plutus script for verifying a ZkFold Symbolic smart contract on the current transaction.
--
-- ZkFold Symbolic smart contracts have access to inputs, reference inputs, outputs and the transaction validity range.
-- Other TxInfo fields can either be passed to the Symbolic contract as private inputs or are not particularly useful inside a ZK contract.
{-# INLINABLE untypedPlonkVerifierTx #-}
untypedPlonkVerifierTx :: SetupBytes -> BuiltinData -> BuiltinUnit
untypedPlonkVerifierTx contract ctx =
    -- Verifying the Plonk `proof` for the `contract` on the transaction data encoded as `input`
    check $ verify @PlonkupPlutus @HaskellCore contract input proof
    where
      -- Extracting transaction data
      ins    = BI.head infoFields                -- txInfoInputs

      refs   = BI.head infBeforeReInputs         -- txInfoReferenceInputs
      refs'  = toBuiltinData . filter (isNothing . txOutReferenceScript . txInInfoResolved)
                 $ unsafeFromBuiltinData @[TxInInfo] refs

      outs   = BI.head infoBeforeOutputs         -- txInfoOutputs
      outs'  = toBuiltinData . initOuts $ unsafeFromBuiltinData @[TxOut] outs

      range  = BI.head $ tail5 infoBeforeOutputs -- txInfoValidRange

      txData = mkTuple4 ins refs' outs' range

      -- Computing public input from the transaction data
      input = toInput . blake2b_224 . BI.serialiseData $ txData

      -- Extract redeemer from ScriptContext
      proof = unsafeFromBuiltinData @ProofBytes $ BI.head $ BI.tail scriptContextTxInfo'

      -- Extracting transaction builtin fields
      scriptContextTxInfo' = BI.snd $ BI.unsafeDataAsConstr ctx
      info                 = BI.head scriptContextTxInfo'
      infoFields           = BI.snd $ BI.unsafeDataAsConstr info
      infBeforeReInputs    = BI.tail infoFields
      infoBeforeOutputs    = BI.tail infBeforeReInputs

      -- Some helper functions
      tail5 = BI.tail . BI.tail . BI.tail . BI.tail . BI.tail

      mkTuple4 a b c d =
        BI.mkList $
          BI.mkCons a $
            BI.mkCons b $
              BI.mkCons c $
                BI.mkCons d $
                  BI.mkNilData BI.unitval

      initOuts [_]    =  []
      initOuts (x:xs) =  x : initOuts xs
      initOuts []     =  traceError "missing change output"
