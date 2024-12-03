module Main where

import           Prelude
import           Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL

import ZkFold.Cardano.OffChain.E2E

main :: IO ()
main = do
    content <- BL.readFile "./test-data/plonk-raw-contract-data.json"
    case decode content :: Maybe IdentityCircuitContract of
        Just jsonData -> putStrLn $ "\n" ++ (show jsonData) ++ "\n"
        Nothing       -> putStrLn "\nFailed to decode JSON.\n"
