module Main where

import           Cardano.Api                           (prettyPrintJSON, unsafeHashableScriptData)
import           Cardano.Api.Shelley                   (fromPlutusData, scriptDataToJsonDetailedSchema)
import qualified Data.Aeson                            as Aeson
import           Data.ByteString                       as BS (writeFile)
import           PlutusLedgerApi.V3                    (fromBuiltin)
import qualified PlutusLedgerApi.V3                    as V3
import           PlutusTx                              (ToData (..))
import           Prelude                               (IO, Integer, Show (..), print, ($), (++), (.))
import           Test.QuickCheck                       (variant)
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)

import           ZkFold.Cardano.Examples.EqualityCheck (equalityCheckVerificationBytes)
import           ZkFold.Cardano.Plonk.OnChain.Utils    (fromInput)

dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

main :: IO ()
main = do
  let seed = 5 :: Integer
  x           <- generate $ variant seed arbitrary
  ps          <- generate $ variant seed arbitrary
  targetValue <- generate $ variant seed arbitrary
  print $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue

  let (_, input, proof) = equalityCheckVerificationBytes x ps targetValue

  BS.writeFile ".././assets/tokenname" $ fromBuiltin $ fromInput input
  BS.writeFile ".././assets/redeemerPlonkVerifier.json" (prettyPrintJSON $ dataToJSON proof)
