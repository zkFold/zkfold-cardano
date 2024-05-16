{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Bench.Scripts                               (compiledPlonkVerifier, compiledPlonkVerify, compiledSymbolicVerifier)
import           Cardano.Api                                 (File (..), IsPlutusScriptLanguage, PlutusScript, PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Shelley                         (PlutusScript (..))
import           Control.Monad                               (void)
import           Data.Aeson                                  (decode)
import           Data.ByteString                             as BS (writeFile)
import qualified Data.ByteString.Lazy                        as BL
import           Data.Map                                    (fromList)
import           Flat                                        (flat)
import qualified PlutusLedgerApi.V3                          as PlutusV3
import           PlutusTx                                    (CompiledCode, ToData (..))
import qualified PlutusTx                                    as P
import qualified PlutusTx                                    as Tx
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           UntypedPlutusCore                           (UnrestrictedProgram (..))

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..), PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk.OffChain               (Contract (..), Plonk32, RowContractJSON, mkInput, mkProof, mkSetup, toContract)
import           ZkFold.Cardano.ScriptsVerifier              (DatumVerifier (..), RedeemerVerifier (..))
import           ZkFold.Symbolic.Cardano.Types               (TxId (..))
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit  (applyArgs)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Types                       (Symbolic)

writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath = let filePath' = ("./assets/" <> filePath <> ".plutus") in
  writePlutusScriptToFile @PlutusScriptV3 filePath' . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

saveFlat redeemer filePath code =
   BS.writeFile ("./assets/" <> filePath <> ".flat") . flat . UnrestrictedProgram <$> P.getPlcNoAnn $ code
           `Tx.unsafeApplyCode` Tx.liftCodeDef (toBuiltinData DatumVerifier) -- we need any unit.json type
           `Tx.unsafeApplyCode` Tx.liftCodeDef (toBuiltinData redeemer)
           -- `Tx.unsafeApplyCode` Tx.liftCodeDef context

lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => TxId a' -> TxId a -> Bool a
lockedByTxId (TxId targetId) (TxId txId) = txId == fromConstant targetId

main :: IO ()
main = do
  savePlutus "symbolicVerifier" compiledSymbolicVerifier
  savePlutus "plonkVerifier"    compiledPlonkVerifier
  savePlutus "plonkVerify"      compiledPlonkVerify
  jsonRowContract <- BL.readFile "test-data/raw-contract-data.json"
  let maybeRowContract = decode jsonRowContract :: Maybe RowContractJSON
  case maybeRowContract of
    Just rowContract ->
      let Contract{..} = toContract rowContract
          Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) @Fr (TxId targetId))
          acc = applyArgs ac [targetId]

          (omega, k1, k2) = getParams 5
          inputs  = fromList [(acOutput acc, 1)]
          plonk   = Plonk omega k1 k2 inputs acc x
          setup'  = setup @Plonk32 plonk
          w       = (PlonkWitnessInput inputs, ps)
          (input', proof') = prove @Plonk32 setup' w
      in do
        let setup = mkSetup setup'
            input = mkInput input'
            proof = mkProof setup proof'
            redeemer = RedeemerVerifier setup input proof
        saveFlat redeemer "plonkSymbolicVerifier" compiledSymbolicVerifier
        saveFlat redeemer "plonkVerifierScript"   compiledPlonkVerifier
        saveFlat redeemer "plonkVerifyScript"     compiledPlonkVerify
    _ -> print ("Could not deserialize" :: String)
