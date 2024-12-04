module Main where

import           Data.Aeson                  (eitherDecode)
import qualified Data.ByteString.Lazy        as BL
import           Prelude

import           ZkFold.Cardano.OffChain.E2E

main :: IO ()
main = do
    content <- BL.readFile "./test-data/plonk-raw-contract-data.json"
    case eitherDecode content :: Either String IdentityCircuitContract of
        Right jsonData -> putStrLn $ "\n" ++ (show jsonData) ++ "\n"
        Left err       -> putStrLn $ "\nFailed to decode JSON: " ++ err ++ "\n"
