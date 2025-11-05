module Main (main) where

import           Data.Bits                    (shiftR, (.&.))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base16       as B16
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Numeric.Natural              (Natural)
import           PlutusTx.Builtins            (BuiltinByteString, fromBuiltin, lengthOfByteString, toBuiltin)
import           Prelude
import           Test.Tasty
import           Test.Tasty.HUnit

import           ZkFold.Cardano.UPLC.Asterizm (buildCrosschainHash)

------------- :Helpers: -------------

-- | Convert BuiltinByteString into hex string
toHex :: BuiltinByteString -> String
toHex = T.unpack . TE.decodeUtf8 . B16.encode. fromBuiltin

-- | Convert hex string into a BuiltinByteString
fromHex :: String -> BuiltinByteString
fromHex str =
  let bytesE = B16.decode . TE.encodeUtf8 $ T.pack str
  in
    case bytesE of
      Left e      -> error e
      Right bytes -> toBuiltin bytes

-- | Big-endian unsigned integer
beUInt :: Int -> Natural -> ByteString
beUInt n x
  | n <= 0    = BS.empty
  | otherwise = BS.pack [ byte i | i <- [n-1, n-2 .. 0] ]
  where
    byte i = fromIntegral ((x `shiftR` (8*i)) .&. 0xFF)

-------------- :Data: ---------------

srcChainId :: BuiltinByteString
srcChainId = toBuiltin $ beUInt 8 0x01

srcAddress :: BuiltinByteString
srcAddress = toBuiltin $ beUInt 32 0x39d2ba91296029aFBE725436B4824cA803e27391

dstChainId :: BuiltinByteString
dstChainId = toBuiltin $ beUInt 8 0x38

dstAddress :: BuiltinByteString
dstAddress = toBuiltin $ beUInt 32 0x39d2ba91296029aFBE725436B4824cA803e27391

txId :: BuiltinByteString
txId = toBuiltin $ beUInt 32 0x01

payload1, payload2 :: BuiltinByteString

payload1 = fromHex "01"
payload2 = fromHex "00000000000000000000000039d2ba91296029afbe725436b4824ca803e2739100000000000000000000000000000000000000000000000000000000000000640000000000000000000000000000000000000000000000000000000000000001"

t0, t1, t2 :: BuiltinByteString

t0 = fromHex "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
t1 = srcChainId <> srcAddress <> dstChainId <> dstAddress <> txId <> payload1
t2 = srcChainId <> srcAddress <> dstChainId <> dstAddress <> txId <> payload2


-- | Hash results thrown by Solidity version of 'buildCrosschainHash'
-- (https://github.com/Asterizm-Protocol/asterizm-contracts-evm/blob/master/contracts/libs/AsterizmHashLib.sol)
v0, v1, v2 :: String

'0':'x' : v0 = "0x1f9d6a75afd516c4cc249e2a28e30e0b51f93915dc7ff0ce74b0e5c8b4dd831a"
'0':'x' : v1 = "0x412ffcb5f599bd658a6db65ff8612756e9a5508dd274ded66d117f9eb29873e1"
'0':'x' : v2 = "0xf8eae6a7072d95317346e49e4b5d11f42b6e6cdad22f0ea5ead1e2e11fd03502"

-------------- :Test: ---------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "buildCrosschainHash"
    [ testGroup "table"
        ( [ testCase ("case " ++ show i) $
              (toHex . buildCrosschainHash) t @?= v
          | (i, (t, v)) <- zip [1..] cases
          ]
        )
    ]
  where
    cases =
      [ (t0, v0)
      , (t1, v1)
      , (t2, v2)
      ]
