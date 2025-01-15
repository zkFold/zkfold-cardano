module Main where

import           Cardano.Api                           (prettyPrintJSON)
import           Data.Aeson                            (decode)
import           Data.ByteString                       as BS (writeFile)
import qualified Data.ByteString.Lazy                  as BL
import           Data.Maybe                            (fromJust)
import           Prelude                               (IO, Show (..), putStr, ($), (++), (.), (<$>))

import           ZkFold.Cardano.Examples.EqualityCheck (EqualityCheckContract (..), equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils         (dataToJSON)

main :: IO ()
main = do
  EqualityCheckContract{..} <- fromJust . decode <$> BL.readFile "test-data/symbolic-raw-contract-data.json"

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n"

  let (_, _, proof) = equalityCheckVerificationBytes x ps targetValue

  BS.writeFile "../../assets/unit.json" $ prettyPrintJSON $ dataToJSON ()
  BS.writeFile "../../assets/redeemerPlonkVerifierTx.json" $ prettyPrintJSON $ dataToJSON proof
