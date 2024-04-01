{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell  #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Symbolic.Verifier where

import           GHC.ByteOrder                            (ByteOrder(..))
import           GHC.Natural                              (naturalToInteger)
import           PlutusLedgerApi.V1.Value                 (flattenValue)
import           PlutusLedgerApi.V3                       (ScriptContext (..), TxInfo (..), TxInInfo (..), TokenName (..))
import           PlutusLedgerApi.V3.Contexts              (ownCurrencySymbol)
import           PlutusTx                                 (CompiledCode, toBuiltinData)
import           PlutusTx.Builtins                        hiding (head)
import           PlutusTx.Prelude                         (Eq (..), Bool (..), Maybe (..), Ord (..), ($), (||), (&&))
import qualified PlutusTx.Prelude                         as Plutus
import           PlutusTx.TH                              (compile)
import           Prelude                                  (undefined, fromInteger, head, (.))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Protocol.ARK.Plonk           (Plonk)
import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof(..))
import           ZkFold.Symbolic.Verifier.Internal

type PlonkBBS = Plonk BuiltinByteString

data PlonkPlutus

mkSetup :: Setup PlonkBBS -> Setup PlonkPlutus
mkSetup ((_, gs, h0, h1, omega, k1, k2), _, (cmQl, cmQr, cmQo, cmQm, cmQc, cmS1, cmS2, cmS3), _, _) =
    ((naturalToInteger $ order @PlonkBBS, convertG1 $ head gs, convertG2 h0, convertG2 h1, convertF omega, convertF k1, convertF k2),
    (convertG1 cmQl, convertG1 cmQr, convertG1 cmQo, convertG1 cmQm, convertG1 cmQc, convertG1 cmS1, convertG1 cmS2, convertG1 cmS3))

mkInput :: Input PlonkBBS -> Input PlonkPlutus
mkInput = convertF . head

mkProof :: Proof PlonkBBS -> Proof PlonkPlutus
mkProof (cmA, cmB, cmC, cmZ, cmT1, cmT2, cmT3, proof1, proof2, a_xi, b_xi, c_xi, s1_xi, s2_xi, z_xi) =
    (convertG1 cmA, convertG1 cmB, convertG1 cmC, convertG1 cmZ, convertG1 cmT1, convertG1 cmT2, convertG1 cmT3,
    convertG1 proof1, convertG1 proof2, convertF a_xi, convertF b_xi, convertF c_xi, convertF s1_xi, convertF s2_xi, convertF z_xi)

instance NonInteractiveProof PlonkPlutus where
    type Transcript PlonkPlutus   = BuiltinByteString
    type Setup PlonkPlutus        = ((Integer, G1, G2, G2, F, F, F), (G1, G1, G1, G1, G1, G1, G1, G1))
    type Witness PlonkPlutus      = ()
    type Input PlonkPlutus        = F
    type Proof PlonkPlutus        = (G1, G1, G1, G1, G1, G1, G1, G1, G1, F, F, F, F, F, F)

    setup :: PlonkPlutus -> Setup PlonkPlutus
    setup = undefined

    prove :: Setup PlonkPlutus -> Witness PlonkPlutus -> (Input PlonkPlutus, Proof PlonkPlutus)
    prove = undefined

    -- TODO: Validate arguments
    {-# INLINABLE verify #-}
    verify :: Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> Bool
    verify
        ((n, g0, h0, h1, omega, k1, k2), (cmQl, cmQr, cmQo, cmQm, cmQc, cmS1, cmS2, cmS3))
        pubInput
        (cmA, cmB, cmC, cmZ, cmT1, cmT2, cmT3, proof1, proof2, a_xi, b_xi, c_xi, s1_xi, s2_xi, z_xi) = bls12_381_finalVerify p1 p2
        where
            (beta, ts) = challenge $ emptyByteString
                `transcriptG1` cmA
                `transcriptG1` cmB
                `transcriptG1` cmC
            (gamma, ts') = challenge ts

            (alpha, ts'') = challenge $ ts' `transcriptG1` cmZ

            (xi, ts''') = challenge $ ts''
                `transcriptG1` cmT1
                `transcriptG1` cmT2
                `transcriptG1` cmT3

            (v, ts'''') = challenge $ ts'''
                `transcriptF` a_xi
                `transcriptF` b_xi
                `transcriptF` c_xi
                `transcriptF` s1_xi
                `transcriptF` s2_xi
                `transcriptF` z_xi

            (u, _) = challenge $ ts''''
                `transcriptG1` proof1
                `transcriptG1` proof2

            zH_xi        = xi `powMod` n - one
            lagrange1_xi = omega * zH_xi / (fromInteger n * (xi - omega))
            omega2       = omega * omega
            lagrange2_xi = omega2 * zH_xi / (F n * (xi - omega2))
            pubPoly_xi   = pubInput * lagrange1_xi - lagrange2_xi

            r0 =
                  pubPoly_xi
                - alpha * alpha * lagrange1_xi
                - alpha
                    * (a_xi + beta * s1_xi + gamma)
                    * (b_xi + beta * s2_xi + gamma)
                    * (c_xi + gamma)
                    * z_xi
            d  =
                  mul (a_xi * b_xi) cmQm
                + mul a_xi cmQl
                + mul b_xi cmQr
                + mul c_xi cmQo
                + cmQc
                + mul (
                          alpha
                        * (a_xi + beta * xi + gamma)
                        * (b_xi + beta * k1 * xi + gamma)
                        * (c_xi + beta * k2 * xi + gamma)
                    +     alpha * alpha * lagrange1_xi
                    +     u
                    ) cmZ
                - mul (
                      alpha
                    * beta
                    * (a_xi + beta * s1_xi + gamma)
                    * (b_xi + beta * s2_xi + gamma)
                    * z_xi
                    ) cmS3
                - mul zH_xi (cmT1 + (xi `powMod` n) `mul` cmT2 + (xi `powMod` (2 Plutus.* n)) `mul` cmT3)
            f  =
                  d
                + v `mul` cmA
                + (v * v) `mul` cmB
                + (v * v * v) `mul` cmC
                + (v * v * v * v) `mul` cmS1
                + (v * v * v * v * v) `mul` cmS2
            e  = (
                - r0
                + v * a_xi
                + v * v * b_xi
                + v * v * v * c_xi
                + v * v * v * v * s1_xi
                + v * v * v * v * v * s2_xi
                + u * z_xi
                ) `mul` g0

            p1 = bls12_381_millerLoop (xi `mul` proof1 + (u * xi * omega) `mul` proof2 + f - e) h0
            p2 = bls12_381_millerLoop (proof1 + u `mul` proof2) h1

-- TODO: split the setup data into the fixed and varying parts
-- | The Plutus script for verifying a ZkFold Symbolic smart contract.
{-# INLINABLE symbolicVerifier #-}
symbolicVerifier :: (Setup PlonkPlutus, Input PlonkPlutus, Proof PlonkPlutus) -> ScriptContext -> Bool
symbolicVerifier (contract, input, proof) ctx = condition1 && condition2
    where
        info  = scriptContextTxInfo ctx
        ins   = Plutus.map txInInfoOutRef (txInfoInputs info)
        outs  = txInfoOutputs info
        refs  = Plutus.map txInInfoOutRef (txInfoReferenceInputs info)
        range = txInfoValidRange info

        h     = blake2b_224 . serialiseData . toBuiltinData $ (ins, refs, outs, range)

        -- Verifying that the public input in the ZKP protocol corresponds to the hash of the transaction data.
        --
        -- ZkFold Symbolic smart contracts will have access to inputs, reference inputs, outputs and the transaction validity range.
        -- Other TxInfo fields can either be passed to the Symbolic contract as private inputs or are not particularly useful inside a contract.
        -- For inputs and reference inputs, we only need the references as we can supply the past transaction data as private inputs.
        condition1 = serialiseData (toBuiltinData input) == h

        -- Verifying the validity of the ZkFold Symbolic smart contract on the current transaction.
        -- The smart contract is encoded into the `Setup PlonkPlutus` data structure.
        condition2 = verify @PlonkPlutus contract input proof

compiledSymbolicVerifier :: CompiledCode ((Setup PlonkPlutus, Input PlonkPlutus, Proof PlonkPlutus) -> ScriptContext -> Bool)
compiledSymbolicVerifier = $$(compile [|| symbolicVerifier ||])

-- | The Plutus script (minting policy) for verifying a Plonk proof.
{-# INLINABLE plonkVerifier #-}
plonkVerifier :: (Setup PlonkPlutus, Input PlonkPlutus, Proof PlonkPlutus) -> ScriptContext -> Bool
plonkVerifier (computation, input, proof) ctx = condition1 || condition2
    where
        info           = scriptContextTxInfo ctx
        Just (_, _, n) = Plutus.find
            (
                \(s, t, _) ->
                s == ownCurrencySymbol ctx
                && TokenName (integerToByteString BigEndian 0 (toF input)) == t
            )
            (flattenValue $ txInfoMint info)

        -- With this minting policy, we can mint tokens if the Plonk proof is valid for the input provided in the redeemer.
        -- The tokens serve as proof that the network has verified the computation.
        -- We can also burn already minted tokens.

        -- Burning already minted tokens.
        condition1 = n < 0

        -- Verifying the Plonk proof.
        condition2 = verify @PlonkPlutus computation input proof

compiledPlonkVerifier :: CompiledCode ((Setup PlonkPlutus, Input PlonkPlutus, Proof PlonkPlutus) -> ScriptContext -> Bool)
compiledPlonkVerifier = $$(compile [|| plonkVerifier ||])