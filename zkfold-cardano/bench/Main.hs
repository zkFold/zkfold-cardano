{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
module Main (main) where

import           Data.Aeson                  (decode)
import           Data.ByteString             as BS (writeFile)
import qualified Data.ByteString.Lazy        as BL
import           Data.Word                   ()
import           Flat                        (flat)
import qualified PlutusTx                    as P
import qualified PlutusTx                    as Tx
import           Prelude                     (IO, Maybe (..), print, ($), (.), (<$>))
import           Script                      (compiledPlonkVerifier, compiledSymbolicVerifier)
import           UntypedPlutusCore           (UnrestrictedProgram (..))

import           ZkFold.Cardano.Plonk.Inputs

-- This reads the test vectors and applies them to the compiled plonk verifier script.
-- This applied verifier is written to disk in the flat format.
-- use: nix shell nixpkgs#flamegraph github:input-output-hk/plutus#x86_64-linux.plutus.library.plutus-project-92.hsPkgs.plutus-core.components.exes.traceToStacks github:input-output-hk/plutus#x86_64-linux.plutus.library.plutus-project-92.hsPkgs.plutus-core.components.exes.uplc
-- use: nix shell nixpkgs#flamegraph github:perturbing/plutus/8152f746f07c2097ef18bd649e55a6269d6cb47b#x86_64-linux.plutus.library.plutus-project-92.hsPkgs.plutus-core.components.exes.traceToStacks github:perturbing/plutus/8152f746f07c2097ef18bd649e55a6269d6cb47b#x86_64-linux.plutus.library.plutus-project-92.hsPkgs.plutus-core.components.exes.uplc
-- and: uplc evaluate -t -i appliedPlonkScript.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs
-- to log the CPU/MEM consumption
-- for more info see https://hydra.family/head-protocol/benchmarks/profiling/
-- and https://plutus.readthedocs.io/en/latest/howtos/profiling-scripts.html

-- Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> Bool
main :: IO ()
main = do
  jsonDataProof <- BL.readFile "test-data/proof.json"
  jsonDataSetup <- BL.readFile "test-data/setup.json"
  jsonDataInput <- BL.readFile "test-data/input.json"
  let maybeProof = decode jsonDataProof :: Maybe ProofJSON
  let maybeSetup = decode jsonDataSetup :: Maybe SetupJSON
  let maybeInput = decode jsonDataInput :: Maybe InputJSON
  case (maybeProof, maybeSetup, maybeInput) of
    (Just prf, Just stp, Just inp) -> do
      let p = convertProofPlonkPlutus prf
      let s = convertSetupPlonkPlutus stp
      let i = convertInputPlonkPlutus inp
      BS.writeFile "symbolicVerifierScript.flat" . flat . UnrestrictedProgram <$> P.getPlcNoAnn $ compiledSymbolicVerifier
         `Tx.unsafeApplyCode` Tx.liftCodeDef s
         `Tx.unsafeApplyCode` Tx.liftCodeDef i
         `Tx.unsafeApplyCode` Tx.liftCodeDef p
      BS.writeFile "plonkVerifierScript.flat" . flat . UnrestrictedProgram <$> P.getPlcNoAnn $ compiledPlonkVerifier
         `Tx.unsafeApplyCode` Tx.liftCodeDef s
         `Tx.unsafeApplyCode` Tx.liftCodeDef i
         `Tx.unsafeApplyCode` Tx.liftCodeDef p
    _ -> print "Could not deserialize"
