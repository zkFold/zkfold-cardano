module ZkFold.Cardano.UPLC.Wallet.Internal (base64urlEncode, bsAsInteger, showInteger) where

import           PlutusLedgerApi.V3
import           PlutusTx.Builtins
import           PlutusTx.Prelude


{-# INLINEABLE showInteger #-}
showInteger :: Integer -> BuiltinByteString
showInteger int = integerToByteString BigEndian 0 $ foldr (\w acc -> w + acc * 256) 0 conv
    where
        conv :: [Integer]
        conv = fmap toAscii $ go int 
            where
                go n
                  | n == 0 = []
                  | otherwise = (n `modulo` 10) : go (n `divide` 10) 

        toAscii :: Integer -> Integer
        toAscii n = n + 48 


{-# INLINEABLE bsAsInteger #-}
bsAsInteger :: BuiltinByteString -> BuiltinByteString
bsAsInteger bs = showInteger $ byteStringToInteger BigEndian bs


{-# INLINEABLE base64urlEncode #-}
base64urlEncode :: BuiltinByteString -> BuiltinByteString
base64urlEncode bs = integerToByteString BigEndian w64Count $ foldr (\w acc -> w + acc * 256) 0 conv
    where
        pad :: Integer
        pad = case (lengthOfByteString bs * 8) `modulo` 6 of
                m | m == 1 -> 32
                m | m == 2 -> 16
                m | m == 3 -> 8
                m | m == 4 -> 4
                m | m == 5 -> 2 
                _          -> 1

        bsInt :: Integer
        bsInt = byteStringToInteger BigEndian bs * pad

        w64Count :: Integer
        w64Count = (lengthOfByteString bs * 8 + 5) `divide` 6

        conv :: [Integer]
        conv = fmap toAscii $ go w64Count bsInt
            where
                go i n
                  | i == 0 = []
                  | otherwise = (n `modulo` 64) : go (i - 1) (n `divide` 64) 

        toAscii :: Integer -> Integer
        toAscii n 
          | n <= 25 = n + 65
          | n <= 51 = n + 71
          | n <= 61 = n - 4
          | n == 62 = 45
          | otherwise = 95

