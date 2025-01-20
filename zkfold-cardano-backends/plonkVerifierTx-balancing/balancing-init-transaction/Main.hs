module Main where

import           Data.Aeson                              (encode)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Prelude                                 (Bool (..), IO, Show (..), putStr, ($), (++))
import           System.Directory                        (createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath                         (takeFileName, (</>))
import           Test.QuickCheck.Arbitrary               (Arbitrary (..))
import           Test.QuickCheck.Gen                     (generate)

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (dataToCBOR, savePlutus)
import           ZkFold.Cardano.UPLC.Common              (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.PlonkVerifierTx     (plonkVerifierTxCompiled')

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "plonkVerifierTx-balancing" -> ".." </> ".."
        "backends"           -> ".."
        "e2e-test"            -> ".."
        _                    -> "."

  createDirectoryIfMissing True $ path </> "test-data"
  createDirectoryIfMissing True $ path </> "assets"

  x  <- generate arbitrary
  ps <- generate arbitrary

  let contract = IdentityCircuitContract x ps

  BL.writeFile (path </> "test-data" </> "plonkVerifierTx-contract-data.json") $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n\n"

  -- let input         = F 26217937587563095239723870254092982918845276250263818911301829349969290592256  -- an arbitrary value
  let (setup, _, _) = identityCircuitVerificationBytes x ps

  let assetsPath = path </> "assets"

  savePlutus (assetsPath </> "plonkVerifierTx.plutus") $ plonkVerifierTxCompiled' setup
  savePlutus (assetsPath </> "parkingSpot.plutus") $ parkingSpotCompiled 54

  BS.writeFile (assetsPath </> "unit.cbor") $ dataToCBOR ()
