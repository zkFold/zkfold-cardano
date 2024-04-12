module Tests.Data (verifyIsSatisfied) where

import           Data.Aeson                                  (decode)
import qualified Data.ByteString.Lazy                        as BL
import           Data.Map                                    (fromList)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           Test.Hspec                                  (describe, hspec, it)
import           Test.QuickCheck                             (Testable (..))

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..), PlonkProverSecret, PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                        (Plonk32, PlonkPlutus, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.Plonk.Inputs                 (Contract (..), RowContractJSON, toContract)
import           ZkFold.Symbolic.Cardano.Types.Tx            (TxId (..))
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit  (applyArgs)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Conditional            (bool)
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Types                       (I, Symbolic)

lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => TxId a' -> TxId a -> Bool a
lockedByTxId (TxId targetId) (TxId txId) = txId == fromConstant targetId

verifyIsSatisfied :: IO ()
verifyIsSatisfied = do
    jsonRowContract <- BL.readFile "test-data/rowcontract.json"
    let maybeRowContract = decode jsonRowContract :: Maybe RowContractJSON
    case maybeRowContract of
      Just rowContract -> do
        let Contract{..} = toContract rowContract
            Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) @Fr (TxId targetId))
            acc = applyArgs ac [targetId]

            (omega, k1, k2) = getParams 5
            inputs  = fromList [(acOutput acc, 1)]
            plonk   = Plonk omega k1 k2 inputs acc x
            setup'  = setup @Plonk32 plonk
            w       = (PlonkWitnessInput inputs, ps)
            (input', proof') = prove @Plonk32 setup' w
         in hspec $ do
             describe "Verifier test (validating data)" $
               it "it must be TRUE" $ verify @PlonkPlutus (mkSetup setup') (mkInput input') (mkProof (mkSetup setup') proof')
      _ -> print "Could not deserialize"
