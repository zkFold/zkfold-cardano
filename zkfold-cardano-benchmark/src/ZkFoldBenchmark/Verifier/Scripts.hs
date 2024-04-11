{-# LANGUAGE TemplateHaskell #-}

module ZkFoldBenchmark.Verifier.Scripts (symbolicVerifierScript, plonkVerifierScript, verifyPlonkScript) where

import           PlutusCore                               (DefaultFun, DefaultUni)
import           PlutusTx                                 (compile, getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                         (($))
import qualified UntypedPlutusCore                        as UPLC

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain                   (plonkVerifier, symbolicVerifier)
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)

symbolicVerifierScript :: Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
symbolicVerifierScript s i p =
    getPlcNoAnn $ $$(compile [|| symbolicVerifier ||])
       `unsafeApplyCode` liftCodeDef s
       `unsafeApplyCode` liftCodeDef i
       `unsafeApplyCode` liftCodeDef p

plonkVerifierScript :: Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
plonkVerifierScript s i p =
    getPlcNoAnn $ $$(compile [|| plonkVerifier ||])
       `unsafeApplyCode` liftCodeDef s
       `unsafeApplyCode` liftCodeDef i
       `unsafeApplyCode` liftCodeDef p

verifyPlonkScript :: Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
verifyPlonkScript s i p =
    getPlcNoAnn $ $$(compile [|| verify @PlonkPlutus ||])
       `unsafeApplyCode` liftCodeDef s
       `unsafeApplyCode` liftCodeDef i
       `unsafeApplyCode` liftCodeDef p
