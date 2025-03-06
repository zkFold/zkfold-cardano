module ZkFold.Cardano.OffChain.BLS12_381 where

import           GHC.Natural                                 (naturalToInteger)
import           PlutusTx.Builtins                           (BuiltinByteString, Integer, toBuiltin)
import           PlutusTx.Prelude                            ((.))

import           ZkFold.Base.Algebra.Basic.Field             (Zp, fromZp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, BLS12_381_G2_Point)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (compress)
import           ZkFold.Base.Data.ByteString                 (toByteString)

convertZp :: Zp p -> Integer
convertZp = naturalToInteger . fromZp

convertG1 :: BLS12_381_G1_Point -> BuiltinByteString
convertG1 = toBuiltin . toByteString . compress

convertG2 :: BLS12_381_G2_Point -> BuiltinByteString
convertG2 = toBuiltin . toByteString . compress
