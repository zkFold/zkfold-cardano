{-
{-# LANGUAGE OverloadedStrings #-}
-}
module Main (main) where

import           Prelude   

{-
import           Bench.Scripts                         (compiledPlonkVerifier, compiledSymbolicVerifier)
import           Cardano.Api                           (File (..), IsPlutusScriptLanguage, PlutusScript, PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Shelley                   (PlutusScript (..))
import           Control.Monad                         (void)
import           Data.Aeson                            (decode)
import           Data.ByteString                       as BS (writeFile)
import qualified Data.ByteString.Lazy                  as BL
import           Flat                                  (flat)
import qualified PlutusLedgerApi.V3                    as PlutusV3
import           PlutusTx                              (CompiledCode, ToData (..))
import qualified PlutusTx                              as P
import qualified PlutusTx                              as Tx
import           Prelude                               hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           UntypedPlutusCore                     (UnrestrictedProgram (..))

import           ZkFold.Cardano.Examples.EqualityCheck (equalityCheckVerificationBytes)
import           ZkFold.Cardano.Plonk.OffChain         (Contract (..), RowContractJSON, toContract)

saveFlat redeemer filePath code =
   BS.writeFile ("./assets/" <> filePath <> ".flat") . flat . UnrestrictedProgram <$> P.getPlcNoAnn $ code
           `Tx.unsafeApplyCode` Tx.liftCodeDef (toBuiltinData redeemer)
-}

main :: IO ()
main = do
  pure ()
  {-
  jsonRowContract <- BL.readFile "test-data/raw-contract-data.json"
  let maybeRowContract = decode jsonRowContract :: Maybe RowContractJSON
  case maybeRowContract of
    Just rowContract ->
      let Contract{..} = toContract rowContract
      in do
        let (setup, input, proof) = equalityCheckVerificationBytes x ps targetValue
            redeemer = (setup, input, proof)
        savePlutus "symbolicVerifier" $ compiledSymbolicVerifier setup
        savePlutus "plonkVerifier"    $ compiledPlonkVerifier setup
        savePlutus "plonkVerify"      compiledPlonkVerify
        saveFlat proof "plonkSymbolicVerifier" $ compiledSymbolicVerifier setup
        saveFlat proof "plonkVerifierScript"   $ compiledPlonkVerifier setup
        saveFlat redeemer "plonkVerifyScript"  $ compiledPlonkVerify `Tx.unsafeApplyCode` Tx.liftCodeDef (toBuiltinData ())
    _ -> print ("Could not deserialize" :: String)
  -}