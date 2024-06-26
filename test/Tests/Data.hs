module Tests.Data (verifyIsSatisfied) where

import           Data.Aeson                                  (decode)
import           Data.ByteString                             (ByteString)
import qualified Data.ByteString.Lazy                        as BL
import           Data.Map                                    (fromList, keys)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           Test.Hspec                                  (describe, hspec, it)

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Data.Vector                     (Vector(..))
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..), PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                        (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OffChain               (Contract (..), RowContractJSON, mkInput, mkProof, mkSetup, toContract)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit  (applyArgs)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Types                       (Symbolic)

type PlonkBase32 = Plonk 32 1 ByteString

lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => a' -> a -> Bool a
lockedByTxId targetValue inputValue = inputValue == fromConstant targetValue

verifyIsSatisfied :: IO ()
verifyIsSatisfied = do
    jsonRowContract <- BL.readFile "test-data/raw-contract-data.json"
    let maybeRowContract = decode jsonRowContract :: Maybe RowContractJSON
    case maybeRowContract of
      Just rowContract -> do
        let Contract{..} = toContract rowContract
            Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) targetValue)
            acc = applyArgs ac [targetValue]

            (omega, k1, k2) = getParams 5
            inputs  = fromList [(acOutput acc, 1)]
            plonk   = Plonk omega k1 k2 (Vector @1 $ keys inputs) acc x :: PlonkBase32
            sP      = setupProve plonk
            sV      = setupVerify plonk
            w       = (PlonkWitnessInput inputs, ps)
            (input', proof') = prove @PlonkBase32 sP w
         in hspec $ do
             describe "Verifier test (validating data)" $
               it "it must be TRUE" $ verify @PlonkPlutus (mkSetup sV) (mkInput input') (mkProof @32 (mkSetup sV) proof')
      _ -> print "Could not deserialize"
