{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts (symbolicVerifierScript, plonkVerifierScript) where

import           PlutusCore                               (DefaultFun, DefaultUni)
import           PlutusLedgerApi.V3                       (ScriptContext)
import           PlutusTx                                 (compile, getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                         (($))
import qualified UntypedPlutusCore                        as UPLC

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Scripts.ForwardingScripts
import           ZkFold.Cardano.Scripts.PlonkVerifier
import           ZkFold.Cardano.Scripts.SymbolicVerifier
import           ZkFold.Cardano.Plonk.OnChain

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
