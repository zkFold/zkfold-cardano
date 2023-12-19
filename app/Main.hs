{-# LANGUAGE TypeApplications #-}

module Main where

import           Prelude                                     hiding (Bool)

import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import           ZkFold.Prelude                              (writeFileJSON)
import           ZkFold.Symbolic.Arithmetization             (ArithmeticCircuit, acSizeN, acSizeM)
import           ZkFold.Symbolic.Compiler                    (compile)
import           ZkFold.Symbolic.Data.Bool                   (Bool(..))

import           ScriptTemplate                              (scriptTemplate)

scriptFile :: FilePath
scriptFile = "compiled_scripts/script.json"

main :: IO ()
main = do
    let Bool ac = compile @(Zp BLS12_381_Scalar) (scriptTemplate @(ArithmeticCircuit (Zp BLS12_381_Scalar))) :: Bool (ArithmeticCircuit (Zp BLS12_381_Scalar))

    putStrLn "\nCompiling the script...\n"

    putStrLn $ "Number of constraints: " ++ show (acSizeN ac)
    putStrLn $ "Number of variables: "   ++ show (acSizeM ac)
    writeFileJSON scriptFile ac
    putStrLn $ "Script saved: " ++ scriptFile