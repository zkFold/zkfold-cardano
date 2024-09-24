module Bench.Utils where

import           Data.ByteString as BS
import           Prelude


-- | Generate a ByteString occupying exactly `m` bytes of memory
memToBS :: Int -> BS.ByteString
memToBS m
  | m >= 0    = BS.replicate m 0
  | otherwise = error "memToBS: Negative size not allowed"
