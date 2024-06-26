{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Bench.Scripts                               (compiledPlonkVerifier, compiledPlonkVerify, compiledSymbolicVerifier)
import           Cardano.Api                                 (File (..), IsPlutusScriptLanguage, PlutusScript, PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Shelley                         (PlutusScript (..))
import           Control.Monad                               (void)
import           Data.Aeson                                  (decode)
import           Data.ByteString                             as BS (writeFile, ByteString)
import qualified Data.ByteString.Lazy                        as BL
import           Data.Map                                    (fromList, keys)
import           Flat                                        (flat)
import qualified PlutusLedgerApi.V3                          as PlutusV3
import           PlutusTx                                    (CompiledCode, ToData (..))
import qualified PlutusTx                                    as P
import qualified PlutusTx                                    as Tx
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           UntypedPlutusCore                           (UnrestrictedProgram (..))

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Data.Vector                     (Vector(..))
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..), PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk.OffChain               (Contract (..), RowContractJSON, mkInput, mkProof, mkSetup, toContract)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit  (applyArgs)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Types                       (Symbolic)

type PlonkBase32 = Plonk 32 1 ByteString

lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => a' -> a -> Bool a
lockedByTxId targetValue inputValue = inputValue == fromConstant targetValue

writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath = let filePath' = ("./assets/" <> filePath <> ".plutus") in
  writePlutusScriptToFile @PlutusScriptV3 filePath' . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

saveFlat redeemer filePath code =
   BS.writeFile ("./assets/" <> filePath <> ".flat") . flat . UnrestrictedProgram <$> P.getPlcNoAnn $ code
           `Tx.unsafeApplyCode` Tx.liftCodeDef (toBuiltinData redeemer)

main :: IO ()
main = do
  
  jsonRowContract <- BL.readFile "test-data/raw-contract-data.json"
  let maybeRowContract = decode jsonRowContract :: Maybe RowContractJSON
  case maybeRowContract of
    Just rowContract ->
      let Contract{..} = toContract rowContract
          Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) targetValue)
          acc = applyArgs ac [targetValue]

          (omega, k1, k2) = getParams 5
          inputs  = fromList [(acOutput acc, 1)]
          plonk   = Plonk omega k1 k2 (Vector @1 $ keys inputs) acc x :: PlonkBase32
          sP      = setupProve plonk
          sV      = setupVerify plonk
          w       = (PlonkWitnessInput inputs, ps)
          (input', proof') = prove @PlonkBase32 sP w
      in do
        let setup = mkSetup sV
            input = mkInput input'
            proof = mkProof @32 setup proof'
            redeemer = (setup, input, proof)
        savePlutus "symbolicVerifier" $ compiledSymbolicVerifier setup
        savePlutus "plonkVerifier"    $ compiledPlonkVerifier setup
        savePlutus "plonkVerify"      compiledPlonkVerify
        saveFlat proof "plonkSymbolicVerifier" $ compiledSymbolicVerifier setup
        saveFlat proof "plonkVerifierScript"   $ compiledPlonkVerifier setup
        saveFlat redeemer "plonkVerifyScript"  $ compiledPlonkVerify `Tx.unsafeApplyCode` Tx.liftCodeDef (toBuiltinData ())
    _ -> print ("Could not deserialize" :: String)
