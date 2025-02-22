module ZkFold.Cardano.Balancing.Transaction.Plonkup where

import           Cardano.Api                             (prettyPrintJSON)
import           Data.Aeson                              (decode)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.Maybe                              (fromJust)
import           PlutusLedgerApi.V1.Value                (lovelaceValue)
import           PlutusLedgerApi.V3                      (BuiltinData, Extended (..),
                                                          Interval (..), Lovelace (..), LowerBound (..),
                                                          OutputDatum (..), POSIXTime, ToData (..), TxInInfo (..),
                                                          TxOut (..), UpperBound (..))
import qualified PlutusTx.Builtins.Internal              as BI
import           PlutusTx.Prelude                        (blake2b_224, sortBy)
import           Prelude                                 (Bool (..), Either (..), FilePath, IO, Maybe (..), Show (..),
                                                          concat, error, putStr, readFile, return, sequenceA, ($), (++),
                                                          (.), (<$>))
import           System.FilePath                         ((</>))

import           ZkFold.Cardano.Examples.IdentityCircuit (IdentityCircuitContract (..),
                                                          identityCircuitVerificationBytes, stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils           (dataToJSON, outRefCompare, parseAddress,
                                                          parseJsonToTxInInfoList)
import           ZkFold.Cardano.OnChain.BLS12_381        (toInput)
import           ZkFold.Cardano.UPLC.Common              (parkingSpotCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx   (plonkupVerifierTxCompiled)

balancingPlonkup :: FilePath -> IO ()
balancingPlonkup path = do
    let testData = path </> "test-data"
        assets   = path </> "assets"

    IdentityCircuitContract x ps <- fromJust . decode <$> BL.readFile (testData </> "plonkupVerifierTx-contract-data.json")

    let (setup, _, _) = identityCircuitVerificationBytes x ps

    let range   = Interval (LowerBound (NegInf:: Extended POSIXTime) True) (UpperBound PosInf True)
        rangeBD = toBuiltinData range

    -- Inputs
    txin1 <- parseJsonToTxInInfoList [ Nothing ] <$> BL.readFile (assets </> "utxo1.json")
    txin2 <- parseJsonToTxInInfoList [ Just $ plonkupVerifierTxCompiled setup ] <$> BL.readFile (assets </> "utxo2.json")
    let inputsE = [txin1, txin2]

    -- Reference inputs
    txin3 <- parseJsonToTxInInfoList [ Just $ parkingSpotCompiled 54 ] <$> BL.readFile (assets </> "utxo3.json")
    let referencesE = [txin3]

    -- Outputs
    addr1T <- readFile (assets </> "alice.addr")
    let val1 = Lovelace 10000000

    let dataE = do
          txIns  <- concat <$> sequenceA inputsE
          txRefs <- concat <$> sequenceA referencesE
          addr1  <- parseAddress addr1T
          return (txIns, txRefs, addr1)

    let (txIns, txRefs, addr1) = case dataE of
            Left errMsg     -> error $ "Error: " ++ errMsg ++ "\n\n"
            Right (a, b, c) -> (a, b, c)

    let out1   = TxOut addr1 (lovelaceValue val1) NoOutputDatum Nothing
        txOuts = [out1]

    putStr $ "\nInputs:\n" ++ show txIns ++ "\n\n"
    putStr $ "Reference inputs:\n" ++ show txRefs ++ "\n\n"
    putStr $ "Outputs:\n" ++ show txOuts ++ "\n\n"

    let txInsSorted = sortBy (\u v -> outRefCompare (txInInfoOutRef u) (txInInfoOutRef v)) txIns
    let txInsBD  = toBuiltinData txInsSorted
        txRefsBD = toBuiltinData txRefs
        txOutsBD = toBuiltinData txOuts
        txDataBD = mkTuple4 txInsBD txRefsBD txOutsBD rangeBD

    let input = toInput . blake2b_224 . BI.serialiseData $ txDataBD
    putStr $ "Verifier's input: " ++ show input ++ "\n\n"

    putStr "Generating proof...\n\n"
    let (_, _, proof) = stateCheckVerificationBytes x ps input
    BS.writeFile (assets </> "redeemerPlonkupVerifierTx.json") $ prettyPrintJSON $ dataToJSON proof


----- HELPER FUNCTIONS -----

mkTuple4 :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData
mkTuple4 a b c d =
  BI.mkList $
    BI.mkCons a $
      BI.mkCons b $
        BI.mkCons c $
          BI.mkCons d $
            BI.mkNilData BI.unitval



