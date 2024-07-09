module ZkFold.Cardano.Plonk.OnChain.Utils where



import           GHC.ByteOrder                             (ByteOrder (..))
import           PlutusLedgerApi.V3
import           PlutusTx.Builtins
import           PlutusTx.Prelude                          (Bool (..), Eq (..), (.))

import           ZkFold.Cardano.Plonk.OnChain.BLS12_381.F  (F (..), toF)
import           ZkFold.Cardano.Plonk.OnChain.BLS12_381.G1 (G1)
import           ZkFold.Cardano.Plonk.OnChain.Data         (InputBytes (..))

{-# INLINABLE mul #-}
mul :: F -> G1 -> G1
mul (F a) = bls12_381_G1_scalarMul a

-- convert hash into Zp BLS12_381_Scalar
{-# INLINABLE toInput #-}
toInput :: BuiltinByteString -> InputBytes
toInput = InputBytes . toF . byteStringToInteger BigEndian

-- hash transaction data with blake2b_224
{-# INLINABLE dataToBlake #-}
dataToBlake :: ToData a => a -> BuiltinByteString
dataToBlake = blake2b_224 . serialiseData . toBuiltinData

-- https://github.com/IntersectMBO/plutus/issues/6273
{-# INLINABLE eqCredential #-}
eqCredential :: Credential -> ScriptPurpose -> Bool
eqCredential a (Rewarding b) = a == b
eqCredential _ _             = False

-- https://github.com/IntersectMBO/plutus/issues/6273
{-# INLINABLE eqCurrencySymbol #-}
eqCurrencySymbol :: CurrencySymbol -> ScriptPurpose -> Bool
eqCurrencySymbol a (Minting b) = a == b
eqCurrencySymbol _ _           = False
