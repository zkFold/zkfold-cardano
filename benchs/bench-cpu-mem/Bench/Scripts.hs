{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts (symbolicVerifierScript, plonkVerifierScript, verifyPlonkScript, compiledPlonkVerifier, compiledSymbolicVerifier, compiledVerifyPlonk) where

import           PlutusCore                              (DefaultFun, DefaultUni)
import PlutusLedgerApi.V3
    ( ScriptContext, getRedeemer, BuiltinData )
import           PlutusTx                                (compile, getPlcNoAnn, liftCodeDef, unsafeApplyCode, CompiledCode, UnsafeFromData (..))
import           PlutusTx.Prelude                        (($), BuiltinUnit, check, (.))
import qualified UntypedPlutusCore                       as UPLC

import           ZkFold.Cardano.Scripts.PlonkVerifier    (plonkVerifier)
import           ZkFold.Cardano.Scripts.SymbolicVerifier (symbolicVerifier)
import ZkFold.Base.Protocol.NonInteractiveProof (verify, NonInteractiveProof (..))
import ZkFold.Cardano.Plonk.OnChain.Data
    ( SetupBytes, ProofBytes, InputBytes )
import ZkFold.Cardano.Plonk ( PlonkPlutus )
import PlutusLedgerApi.V3.Contexts (ScriptContext(..))

-- bench-cpu-mem

symbolicVerifierScript :: SetupBytes -> ProofBytes -> ScriptContext -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
symbolicVerifierScript paramsSetup redeemerProof ctx =
    getPlcNoAnn $ $$(compile [|| symbolicVerifier ||])
       `unsafeApplyCode` liftCodeDef paramsSetup
       `unsafeApplyCode` liftCodeDef redeemerProof
       `unsafeApplyCode` liftCodeDef ctx

plonkVerifierScript :: SetupBytes -> ProofBytes -> ScriptContext -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
plonkVerifierScript paramsSetup redeemerProof ctx =
    getPlcNoAnn $ $$(compile [|| plonkVerifier ||])
       `unsafeApplyCode` liftCodeDef paramsSetup
       `unsafeApplyCode` liftCodeDef redeemerProof
       `unsafeApplyCode` liftCodeDef ctx

verifyPlonkScript :: SetupBytes -> InputBytes -> ProofBytes -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
verifyPlonkScript paramsSetup input redeemerProof =
    getPlcNoAnn $ $$(compile [|| verify @PlonkPlutus ||])
       `unsafeApplyCode` liftCodeDef paramsSetup
       `unsafeApplyCode` liftCodeDef input
       `unsafeApplyCode` liftCodeDef redeemerProof


-- analyze-uplc

compiledSymbolicVerifier :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledSymbolicVerifier contract =
    $$(compile [|| untypedSymbolicVerifier ||])
    `unsafeApplyCode` liftCodeDef contract
  where
    untypedSymbolicVerifier :: SetupBytes -> BuiltinData -> BuiltinUnit
    untypedSymbolicVerifier contract' ctx' =
      let ctx = unsafeFromBuiltinData ctx' in
      check $
        symbolicVerifier
            contract'
            (unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx)
            ctx

compiledPlonkVerifier :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledPlonkVerifier computation =
    $$(compile [|| untypedPlonkVerifier ||])
    `unsafeApplyCode` liftCodeDef computation
  where
    untypedPlonkVerifier :: SetupBytes -> BuiltinData -> BuiltinUnit
    untypedPlonkVerifier computation' ctx' =
      let ctx = unsafeFromBuiltinData ctx' in
      check $
        plonkVerifier
            computation'
            (unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx)
            ctx

compiledVerifyPlonk :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
compiledVerifyPlonk computation =
    $$(compile [|| untypedVerifyPlonk ||])
    `unsafeApplyCode` liftCodeDef computation
  where
    untypedVerifyPlonk :: SetupBytes -> BuiltinData -> BuiltinData -> BuiltinUnit
    untypedVerifyPlonk computation' input proof =
      check
        ( verify @PlonkPlutus
            computation'
            (unsafeFromBuiltinData input)
            (unsafeFromBuiltinData proof)
        )