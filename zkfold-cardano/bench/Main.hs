{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

import           Data.Aeson                                  (decode)
import           Data.ByteString                             as BS (writeFile)
import qualified Data.ByteString.Lazy                        as BL
import           Data.Map                                    (fromList)
import           Data.Word                                   ()
import           Flat                                        (flat)
import qualified PlutusTx                                    as P
import qualified PlutusTx                                    as Tx
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           Script                                      (compiledPlonkVerifier, compiledSymbolicVerifier)
import           UntypedPlutusCore                           (UnrestrictedProgram (..))

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Protocol.ARK.Plonk              (Plonk (..), PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                        (Plonk32, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.Plonk.Inputs                 (Contract (..), RowContractJSON, toContract)
import           ZkFold.Symbolic.Cardano.Types.Tx            (TxId (..))
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Types                       (Symbolic)

lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => TxId a' -> TxId a -> () -> Bool a
lockedByTxId (TxId targetId) (TxId txId) _ = txId == fromConstant targetId

main :: IO ()
main = do
  jsonRowContract <- BL.readFile "test-data/rowcontract.json"
  let maybeRowContract = decode jsonRowContract :: Maybe RowContractJSON
  case maybeRowContract of
    Just rowContract ->
      let Contract{..} = toContract rowContract
          Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) (TxId targetId))
          (omega, k1, k2) = getParams 5
          inputs  = fromList [(1, targetId), (acOutput ac, 1)]
          plonk   = Plonk omega k1 k2 inputs ac x
          setup'  = setup @Plonk32 plonk
          w       = (PlonkWitnessInput inputs, ps)
          (input', proof') = prove @Plonk32 setup' w
      in do
        let setup = mkSetup setup'
            input = mkInput input'
            proof = mkProof proof'
        BS.writeFile "symbolicVerifierScript.flat" . flat . UnrestrictedProgram <$> P.getPlcNoAnn $ compiledSymbolicVerifier
           `Tx.unsafeApplyCode` Tx.liftCodeDef setup
           `Tx.unsafeApplyCode` Tx.liftCodeDef input
           `Tx.unsafeApplyCode` Tx.liftCodeDef proof
        BS.writeFile "plonkVerifierScript.flat" . flat . UnrestrictedProgram <$> P.getPlcNoAnn $ compiledPlonkVerifier
           `Tx.unsafeApplyCode` Tx.liftCodeDef setup
           `Tx.unsafeApplyCode` Tx.liftCodeDef input
           `Tx.unsafeApplyCode` Tx.liftCodeDef proof
        BS.writeFile "plonkVerifyScript.flat" . flat . UnrestrictedProgram <$> P.getPlcNoAnn $ compiledPlonkVerifier
           `Tx.unsafeApplyCode` Tx.liftCodeDef setup
           `Tx.unsafeApplyCode` Tx.liftCodeDef input
           `Tx.unsafeApplyCode` Tx.liftCodeDef proof
    _ -> print "Could not deserialize"
