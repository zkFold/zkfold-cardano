{-# LANGUAGE TypeApplications    #-}

module Examples.Fibonacci (exampleFibonacci) where

import           Prelude                         hiding (Num(..), Eq(..), Bool, (^), (/), (||), not, any)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field  (Zp)
import           ZkFold.Symbolic.Arithmetization  (compile, acSizeM, acSizeN)
import           ZkFold.Symbolic.Data.Bool        (Bool (..))
import           ZkFold.Symbolic.Data.Conditional (bool)
import           ZkFold.Symbolic.Data.Eq          (Eq (..))
import           ZkFold.Symbolic.Types            (R, I, SmallField, Symbolic)

-- The Fibonacci index function. If `x` is a Fibonacci number, returns its index (up until `nMax`). Otherwise, returns `0`.
fibonacciIndex :: forall a . Symbolic a => Integer -> a -> a
fibonacciIndex nMax x = foldl (\m k -> bool m (fromConstant @I @a k) (fib k one one == x :: Bool a)) zero [1..nMax]
    where
        fib :: I -> a -> a -> a
        fib 1 x1 _  = x1
        fib n x1 x2 = fib (n - 1) x2 (x1 + x2)

exampleFibonacci :: IO ()
exampleFibonacci = do
    let nMax = 10

    let r = compile @(Zp SmallField) (fibonacciIndex @R nMax) :: R

    putStrLn "\nStarting Fibonacci test...\n"

    putStrLn "Fibonacci index function"
    putStrLn "R1CS size:"
    putStrLn $ "Number of constraints: " ++ show (acSizeN r)
    putStrLn $ "Number of variables: " ++ show (acSizeM r)