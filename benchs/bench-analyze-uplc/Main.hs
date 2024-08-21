{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import           Bench.Scripts                         (compiledPlonkVerifier, compiledSymbolicVerifier, compiledVerifyPlonk)
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
import           ZkFold.Cardano.Plonk.OffChain         (EqualityCheckContract (..))
import           Test.QuickCheck.Arbitrary                   (Arbitrary (..))
import           Test.QuickCheck.Gen                         (generate)

saveFlat redeemer filePath code =
   BS.writeFile ("./assets/" <> filePath <> ".flat") . flat . UnrestrictedProgram <$> P.getPlcNoAnn $ code
           `Tx.unsafeApplyCode` Tx.liftCodeDef (toBuiltinData redeemer)

writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode


main :: IO ()
main = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary

    let (setup, input, proof) = equalityCheckVerificationBytes x ps targetValue
        redeemer = (setup, input, proof)

    savePlutus "symbolicVerifier" $ compiledSymbolicVerifier setup
    savePlutus "plonkVerifier"    $ compiledPlonkVerifier setup
    savePlutus "plonkVerify"      $ compiledVerifyPlonk setup
    saveFlat proof "plonkSymbolicVerifier" $ compiledSymbolicVerifier setup
    saveFlat proof "plonkVerifierScript"   $ compiledPlonkVerifier setup
    saveFlat redeemer "plonkVerifyScript"  $ compiledVerifyPlonk setup
