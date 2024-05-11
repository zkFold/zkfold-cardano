{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main (main) where

import           Bench.Scripts                               (compiledPlonkVerifier, compiledPlonkVerify, compiledSymbolicVerifier)
import           Cardano.Api.Shelley                         (fromPlutusData, scriptDataToJsonDetailedSchema, unsafeHashableScriptData)
import           Cardano.Binary                              (serialize')
import           Codec.Serialise                             (Serialise (encode))
import           Data.Aeson                                  (ToJSON, Value, decode)
import           Data.Aeson.Encode.Pretty                    (encodePretty)
import           Data.ByteString                             as BS (ByteString, writeFile)
import qualified Data.ByteString.Base16                      as B16
import qualified Data.ByteString.Lazy                        as BL
import           Data.Map                                    (fromList)
import           Flat                                        (flat)
import           PlutusLedgerApi.Common                      (serialiseCompiledCode)
import           PlutusLedgerApi.V3                          (Datum (..), Interval, POSIXTime, Redeemer (..), TxInInfo, TxOut, always)
import qualified PlutusLedgerApi.V3                          as PlutusV3
import           PlutusTx                                    (CompiledCode, ToData (..))
import qualified PlutusTx                                    as P
import qualified PlutusTx                                    as Tx
import           PlutusTx.Builtins                           (blake2b_256, serialiseData)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           UntypedPlutusCore                           (UnrestrictedProgram (..))

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..), PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk.OffChain               (Contract (..), Plonk32, RowContractJSON, mkInput, mkProof, mkSetup, toContract)
import           ZkFold.Cardano.Plonk.OnChain                (DatumVerifier (..), ParamsVerifier (..), RedeemerVerifier (..))
import           ZkFold.Symbolic.Cardano.Types               (TxId (..))
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit  (applyArgs)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Types                       (Symbolic)

lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => TxId a' -> TxId a -> Bool a
lockedByTxId (TxId targetId) (TxId txId) = txId == fromConstant targetId

savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath code =
    let validator = serialiseCompiledCode code
    in BS.writeFile ("./assets/" <> filePath <> ".plutus")
         $ "{\n  \"type\": \"PlutusScriptV3\",\n  \"description\": \"\",\n  \"cborHex\": \""
         <> B16.encode (serialize' validator) <> "\"\n}"

saveFlat redeemer filePath code =
   BS.writeFile ("./assets/" <> filePath <> ".flat") . flat . UnrestrictedProgram <$> P.getPlcNoAnn $ code
           `Tx.unsafeApplyCode` Tx.liftCodeDef (toBuiltinData DatumVerifier)
           `Tx.unsafeApplyCode` Tx.liftCodeDef (toBuiltinData redeemer)
           -- `Tx.unsafeApplyCode` Tx.liftCodeDef context

prettyPrintJSON :: ToJSON a => a -> ByteString
prettyPrintJSON = BL.toStrict . encodePretty

dataToJSON :: ToData a => a -> Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . PlutusV3.toData

{-# INLINABLE params #-}
params :: ParamsVerifier
params = ParamsVerifier . blake2b_256 . serialiseData . toBuiltinData $
  ( []     :: [TxInInfo]
  , []     :: [TxInInfo]
  , []     :: [TxOut]
  , always :: Interval POSIXTime
  )

main :: IO ()
main = do
  savePlutus "symbolicVerifier" $ compiledSymbolicVerifier params
  savePlutus "plonkVerifier"    $ compiledPlonkVerifier params
  savePlutus "plonkVerify"      $ compiledPlonkVerify
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
        BS.writeFile "./assets/redeemer.json" (prettyPrintJSON $ dataToJSON redeemer)
        BS.writeFile "./assets/redeemer.cbor" (B16.encode (serialize' $ encode (Redeemer . toBuiltinData $ redeemer)))
        BS.writeFile "./assets/datum.json" (prettyPrintJSON $ dataToJSON DatumVerifier)
        BS.writeFile "./assets/datum.cbor" (B16.encode (serialize' $ encode (Datum $ toBuiltinData DatumVerifier)))
        saveFlat redeemer "plonkSymbolicVerifier" $ compiledSymbolicVerifier params
        saveFlat redeemer "plonkVerifierScript"   $ compiledPlonkVerifier params
        saveFlat redeemer "plonkVerifyScript"     $ compiledPlonkVerify
    _ -> print ("Could not deserialize" :: String)
