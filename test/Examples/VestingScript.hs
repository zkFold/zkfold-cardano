{-# LANGUAGE TypeApplications    #-}

module Examples.VestingScript (exampleVesting) where

import           Data.Foldable                   (toList)
import           Prelude                         (IO, ($))

import           ZkFold.Cardano.Types
import           ZkFold.Base.Algebra.Basic.Field (Zp)
import           ZkFold.Symbolic.Arithmetization
import           ZkFold.Symbolic.Compiler        (compileIO)
import           ZkFold.Symbolic.Data.Bool       (BoolType (..), Bool (..), any)
import           ZkFold.Symbolic.Data.Eq         (Eq(..))
import           ZkFold.Symbolic.Data.Ord        (Ord(..))
import           ZkFold.Symbolic.Types           (Symbolic, BLS12_381_Scalar)

vestingScript :: forall a . Symbolic a => Datum a -> Redeemer a -> ScriptContext a -> Bool a
vestingScript datum _ ctx =
    let Datum vestAddr vestTime vestValue = datum

        info  = scriptContextTxInfo ctx
        now   = lowerBound $ txInfoValidRange info
        outs  = toList $ txInfoOutputs info

    in now >= vestTime && any (\(TxOut addr val _) -> (addr == vestAddr) && (val == vestValue)) outs

exampleVesting :: IO ()
exampleVesting = do
    compileIO @(Zp BLS12_381_Scalar) "compiled_scripts/vesting.json"
        (vestingScript @(ArithmeticCircuit (Zp BLS12_381_Scalar)))