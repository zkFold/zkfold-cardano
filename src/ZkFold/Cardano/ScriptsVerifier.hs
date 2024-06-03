{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.ScriptsVerifier where

import           GHC.ByteOrder                            (ByteOrder (..))
import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V1.Value                 (Value (..))
import           PlutusLedgerApi.V3                       (OutputDatum (..), ScriptContext (..), TokenName (..), TxInfo (..), TxOut (..), UnsafeFromData (..))
import           PlutusLedgerApi.V3.Contexts              (TxInInfo (..), ownCurrencySymbol)
import           PlutusTx                                 (makeIsDataIndexed, makeLift, toBuiltinData)
import qualified PlutusTx.AssocMap                        as AssocMap
import           PlutusTx.Builtins                        (blake2b_224, byteStringToInteger, serialiseData)
import           PlutusTx.Prelude                         (Bool (..), Eq (..), Maybe (..), Ord (..), error, ($), (&&), (.), (<$>), (||))

import           ZkFold.Base.Algebra.Basic.Class          (AdditiveGroup (..))
import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (F (..), InputBytes (..), ProofBytes, SetupBytes)

-- How to use?
--
-- Step one: Send the setup to some address in the datum to blockchain.
-- Step two: Use the setup datum already stored in the blockchain network as a reference input.
--
-- DatumSetup is a plonk setup.
newtype DatumSetup = DatumSetup { contract :: SetupBytes }
  deriving stock (Generic)

makeLift ''DatumSetup
makeIsDataIndexed ''DatumSetup [('DatumSetup, 0)]

-- How to use?
--
-- Step one: Сreate a transaction hash.
-- Step two: Send the datum to the script address.
--
-- DatumSymbolic is a hash transaction in plonk input.
newtype DatumSymbolic = DatumSymbolic { input :: InputBytes }
  deriving stock (Generic)

makeLift ''DatumSymbolic
makeIsDataIndexed ''DatumSymbolic [('DatumSymbolic, 0)]

-- How to use?
--
-- Step one: Make a request to the blockchain and ask for DatumSetup and DatumSymbolic.
-- Step two: Create a proof from a setup, public input and private witness.
--
-- RedeemerSymbolic is a plonk proof.
newtype RedeemerSymbolic = RedeemerSymbolic { proof :: ProofBytes }
  deriving stock (Generic)

makeLift ''RedeemerSymbolic
makeIsDataIndexed ''RedeemerSymbolic [('RedeemerSymbolic, 0)]

-- | The Plutus script for verifying a ZkFold Symbolic smart contract.
{-# INLINABLE symbolicVerifier #-}
symbolicVerifier :: DatumSymbolic -> RedeemerSymbolic -> ScriptContext -> Bool
symbolicVerifier (DatumSymbolic input) (RedeemerSymbolic proof) ctx =
    conditionTransaction && conditionVerify
    where
        info  = scriptContextTxInfo ctx

        ins   = txInInfoOutRef <$> txInfoInputs info
        refs  = txInInfoOutRef <$> txInfoReferenceInputs info
        outs  = txInfoOutputs info
        range = txInfoValidRange info

        ---------------------- calculate transaction hash ----------------------

        hash = F . byteStringToInteger BigEndian . blake2b_224 . serialiseData . toBuiltinData $ (ins, refs, outs, range)

        ------------------------------------------------------------------------

        -- Verifying that the public input in the ZKP protocol corresponds to the hash of the transaction data.
        --
        -- ZkFold Symbolic smart contracts will have access to inputs, reference inputs, outputs and the transaction validity range.
        -- Other TxInfo fields can either be passed to the Symbolic contract as private inputs or are not particularly useful inside a contract.
        conditionTransaction = pubInput input == negate hash


        ---------------------- setup from reference input ----------------------

        [input1] = txInfoReferenceInputs info
        (DatumSetup contract) = case txOutDatum $ txInInfoResolved input1 of
          OutputDatum datum -> unsafeFromBuiltinData @DatumSetup $ toBuiltinData datum
          OutputDatumHash _ -> error ()
          NoOutputDatum     -> error ()

        ------------------------------------------------------------------------

        -- Verifying the validity of the ZkFold Symbolic smart contract on the current transaction.
        -- The smart contract is encoded into the `Setup PlonkPlutus` data structure.
        conditionVerify = verify @PlonkPlutus contract input proof

data RedeemerToken = RedeemerToken SetupBytes InputBytes ProofBytes
  deriving stock (Generic)

makeLift ''RedeemerToken
makeIsDataIndexed ''RedeemerToken [('RedeemerToken, 0)]

-- | The Plutus script (minting policy) for verifying a Plonk proof.
{-# INLINABLE tokenVerifier #-}
tokenVerifier :: RedeemerToken -> ScriptContext -> Bool
tokenVerifier (RedeemerToken computation input proof) ctx =
    conditionTokenName && (conditionBurning || conditionVerify)
    where
        info               = scriptContextTxInfo ctx

        ---------------------------- find own tokens ---------------------------

        Just m             = AssocMap.lookup (ownCurrencySymbol ctx) (getValue $ txInfoMint info)
        [(TokenName t, n)] = AssocMap.toList m

        ------------------------------------------------------------------------

        -- With this minting policy, we can mint tokens if the Plonk proof is valid for the input provided in the redeemer.
        -- The tokens serve as proof that the network has verified the computation.
        --
        -- Verifying that the token name equals to the bytestring representation of the public input in the ZKP protocol.
        conditionTokenName = (F . byteStringToInteger BigEndian $ t) == pubInput input

        -- Burning already minted tokens.
        conditionBurning = n < 0

        -- Verifying the Plonk proof.
        conditionVerify = verify @PlonkPlutus computation input proof
