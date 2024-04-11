{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Script (compiledSymbolicVerifier, compiledPlonkVerifier, compiledPlonkVerify) where

import           PlutusLedgerApi.V3                       (ScriptContext (..))
import           PlutusTx                                 (CompiledCode)
import           PlutusTx.Prelude                         (Bool)
import           PlutusTx.TH                              (compile)

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain                   (plonkVerifier, symbolicVerifier)
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)

compiledSymbolicVerifier :: CompiledCode (Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> Bool)
compiledSymbolicVerifier = $$(compile [|| symbolicVerifier ||])

compiledPlonkVerifier :: CompiledCode (Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> Bool)
compiledPlonkVerifier = $$(compile [|| plonkVerifier ||])

compiledPlonkVerify :: CompiledCode (Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> Bool)
compiledPlonkVerify = $$(compile [|| verify @PlonkPlutus ||])
