module ZkFold.Cardano.OffChain.BLS12_381 where

import           GHC.Natural                            (naturalToInteger)
import           PlutusTx.Builtins                      (BuiltinByteString, Integer, toBuiltin)
import           PlutusTx.Prelude                       ((.))

import           ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, BLS12_381_G2_Point)
import           ZkFold.Algebra.EllipticCurve.Class     (compress)
import           ZkFold.Algebra.Field                   (Zp, fromZp)
import           ZkFold.Data.Binary                 (toByteString)

convertZp :: Zp p -> Integer
convertZp = naturalToInteger . fromZp

convertG1 :: BLS12_381_G1_Point -> BuiltinByteString
convertG1 = toBuiltin . toByteString . compress

convertG2 :: BLS12_381_G2_Point -> BuiltinByteString
convertG2 = toBuiltin . toByteString . compress
