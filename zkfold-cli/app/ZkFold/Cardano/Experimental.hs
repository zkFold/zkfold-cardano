module ZkFold.Cardano.Experimental where

import           Prelude
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)

import           ZkFold.Cardano.Examples.EqualityCheck (EqualityCheckContract (..), equalityCheckVerificationBytes)


{-

zkfold-cardano$ cabal repl zkfold-cli
ghci> 
ghci> :m ZkFold.Cardano.Experimental
ghci> checkSetupInvariant 

Invariant setup if only prover secret is modified:
True

Invariant setup if only target value is modified:
False
ghci> 

-}

eqContract :: IO EqualityCheckContract
eqContract = do
  x           <- generate arbitrary
  ps          <- generate arbitrary
  targetValue <- generate arbitrary

  return $ EqualityCheckContract x ps targetValue

checkSetupInvariant :: IO ()
checkSetupInvariant = do
  EqualityCheckContract x ps t <- eqContract
  let (setup1, _, _) = equalityCheckVerificationBytes x ps t

  ps' <- generate arbitrary
  t'  <- generate arbitrary

  let (setup2, _, _) = equalityCheckVerificationBytes x ps' t
      (setup3, _, _) = equalityCheckVerificationBytes x ps t'

  putStr "\n"

  putStrLn "Invariant setup if only prover secret is modified:"
  print $ show setup1 == show setup2

  putStr "\n"

  putStrLn "Invariant setup if only target value is modified:"
  print $ show setup1 == show setup3

