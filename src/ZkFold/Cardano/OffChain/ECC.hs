module ZkFold.Cardano.OffChain.ECC where

import           GHC.Natural                                 (naturalToInteger)
import           PlutusTx.Builtins
import           PlutusTx.Prelude                            ((.))

import           ZkFold.Base.Algebra.Basic.Field             (Zp, fromZp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, BLS12_381_G2)
import           ZkFold.Base.Algebra.EllipticCurve.Class     (Point (..), compress)
import           ZkFold.Base.Data.ByteString                 (toByteString)

convertZp :: Zp p -> Integer
convertZp = naturalToInteger . fromZp

convertG1 :: Point BLS12_381_G1 -> BuiltinByteString
convertG1 = toBuiltin . toByteString . compress

convertG2 :: Point BLS12_381_G2 -> BuiltinByteString
convertG2 = toBuiltin . toByteString . compress
