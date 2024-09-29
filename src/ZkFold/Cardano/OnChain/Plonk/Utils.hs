module ZkFold.Cardano.OnChain.Plonk.Utils where

import           GHC.ByteOrder                             (ByteOrder (..))
import           PlutusLedgerApi.V3
import           PlutusTx.Builtins
import           PlutusTx.Prelude                          ((.))

import           ZkFold.Cardano.OnChain.BLS12_381.F        (F (..))
import           ZkFold.Cardano.OnChain.Plonk.Data         (InputBytes (..))

-- | convert hash into Zp BLS12_381_Scalar
{-# INLINABLE toInput #-}
toInput :: BuiltinByteString -> InputBytes
toInput = InputBytes . F . byteStringToInteger BigEndian

-- | convert Zp BLS12_381_Scalar into hash
{-# INLINABLE fromInput #-}
fromInput :: InputBytes -> BuiltinByteString
fromInput (InputBytes (F input)) = integerToByteString BigEndian 32 input
