module Main where

import           Prelude
import           System.Directory       (createDirectoryIfMissing)

main :: IO ()
main = do
    createDirectoryIfMissing True "compiled_scripts"