{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts (symbolicVerifierScript, plonkVerifierScript, verifyPlonkScript) where

import           PlutusCore                               (DefaultFun, DefaultUni)
import           PlutusLedgerApi.V3                       (ScriptContext)
import           PlutusTx                                 (compile, getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                         (($))
import qualified UntypedPlutusCore                        as UPLC

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (DatumVerifier, ParamsVerifier, RedeemerVerifier (..))
import           ZkFold.Cardano.ScriptsVerifier           (plonkVerifier, symbolicVerifier)

symbolicVerifierScript :: ParamsVerifier -> DatumVerifier -> RedeemerVerifier -> ScriptContext -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
symbolicVerifierScript params datum redeemer ctx =
    getPlcNoAnn $ $$(compile [|| symbolicVerifier ||])
       `unsafeApplyCode` liftCodeDef params
       `unsafeApplyCode` liftCodeDef datum
       `unsafeApplyCode` liftCodeDef redeemer
       `unsafeApplyCode` liftCodeDef ctx

plonkVerifierScript :: ParamsVerifier -> DatumVerifier -> RedeemerVerifier -> ScriptContext -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
plonkVerifierScript params datum redeemer ctx =
    getPlcNoAnn $ $$(compile [|| plonkVerifier ||])
       `unsafeApplyCode` liftCodeDef params
       `unsafeApplyCode` liftCodeDef datum
       `unsafeApplyCode` liftCodeDef redeemer
       `unsafeApplyCode` liftCodeDef ctx

verifyPlonkScript ::  RedeemerVerifier -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
verifyPlonkScript (RedeemerVerifier s i p) =
    getPlcNoAnn $ $$(compile [|| verify @PlonkPlutus ||])
       `unsafeApplyCode` liftCodeDef s
       `unsafeApplyCode` liftCodeDef i
       `unsafeApplyCode` liftCodeDef p
