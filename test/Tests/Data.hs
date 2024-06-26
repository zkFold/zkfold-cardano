module Tests.Data (verifyIsSatisfied) where

import           Data.Aeson                            (decode)
import qualified Data.ByteString.Lazy                  as BL
import           Prelude                               hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           Test.Hspec                            (describe, hspec, it)

import           ZkFold.Cardano.Examples.EqualityCheck (testEqualityCheckContract)
import           ZkFold.Cardano.Plonk.OffChain         (Contract (..), RowContractJSON, toContract)

verifyIsSatisfied :: IO ()
verifyIsSatisfied = do
    jsonRowContract <- BL.readFile "test-data/raw-contract-data.json"
    let maybeRowContract = decode jsonRowContract :: Maybe RowContractJSON
    case maybeRowContract of
      Just rowContract -> do
        let Contract{..} = toContract rowContract
         in hspec $ do
             describe "Verifier test (validating data)" $
               it "it must be TRUE" $ testEqualityCheckContract x ps targetValue
      _ -> print "Could not deserialize"
