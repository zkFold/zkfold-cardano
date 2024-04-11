{-# LANGUAGE TemplateHaskell   #-}

module Script where

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
