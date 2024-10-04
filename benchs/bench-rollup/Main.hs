{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import           Bench.Statistics                      (getCostsCek)
import           Bench.Utils                           (printCSVWithHeaders, writeCSV)
import           Data.Aeson                            (encode)
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as BL
import           Data.String                           (IsString (fromString))
import           PlutusCore                            (DefaultFun, DefaultUni)
import qualified PlutusLedgerApi.V2                    as V2
import           PlutusLedgerApi.V3
import           PlutusTx                              (getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                      (($), (.))
import           Prelude                               hiding (Bool, Eq (..), Fractional (..), Num (..), length, ($),
                                                        (.))
import           System.Directory                      (createDirectoryIfMissing)
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)
import qualified UntypedPlutusCore                     as UPLC

import           ZkFold.Cardano.Examples.EqualityCheck (equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E           (EqualityCheckContract (..))
import           ZkFold.Cardano.OnChain.BLS12_381.F    (F (..))
import           ZkFold.Cardano.OnChain.Plonk.Data     (ProofBytes (..), SetupBytes)
import           ZkFold.Cardano.UPLC                   (rollupCompiled)
import           ZkFold.Cardano.UPLC.Rollup            (RollupRedeemer (..))

sampleRedeemer :: ProofBytes -> Int -> Redeemer
sampleRedeemer proof n = Redeemer . toBuiltinData $ RollupRedeemer
  { rrProof   = proof
  , rrAddress = sampleAddress
  , rrValue   = sampleValue
  , rrState   = sampleState
  , rrUpdate  = replicate n sampleState
  }

unitDatum :: OutputDatum
unitDatum = OutputDatum . Datum . toBuiltin . toData $ ()

{-
sampleDatum :: OutputDatum
sampleDatum = OutputDatum . Datum . toBuiltin . B $
  (fromString "0000000000000000000000000000000000000000000000000000000000000000" :: BS.ByteString)  -- 32 bytes datum
-}

sampleAddress :: Address
sampleAddress = Address (PubKeyCredential . PubKeyHash $
                          toBuiltin (fromString "8b1dd80eb5d1da1afad0ed5a6be7eb9e46481a74621cb7d787caa3fc" :: BS.ByteString)
                        ) Nothing

sampleValue :: V2.Value
sampleValue = V2.singleton V2.adaSymbol V2.adaToken 100000000

sampleState :: F
sampleState = F 25154496976141666116585768883114414650575212708960519991881605349447581737748

sampleTxId :: TxId
sampleTxId = TxId $ toBuiltin (fromString "25923f589a26311e87fb37bb41cb1dadf9a90166775f9f3b303cfe24e4fb95f8" :: BS.ByteString)

-- | Sample script output
sampleScriptTxOut :: TxOut
sampleScriptTxOut = TxOut sampleAddress sampleValue unitDatum Nothing

-- | Sample pub key output
samplePubTxOut :: TxOut
samplePubTxOut = TxOut sampleAddress sampleValue NoOutputDatum Nothing

-- | Sample reference output
sampleReferenceTxOut :: TxOut
sampleReferenceTxOut = TxOut sampleAddress sampleValue unitDatum $
  Just . ScriptHash $ toBuiltin (fromString "264ac730a6d3dacd0be8f9948e161aa151fd49d5e48203c31b2ae5eb" :: BS.ByteString)

dummyRedeemer :: ProofBytes
dummyRedeemer = ProofBytes e e e e e e e e e 0 0 0 0 0 0 (F 0)
  where e = ""

contextRollup :: ProofBytes -> Int -> ScriptContext
contextRollup proof n = ScriptContext
  { scriptContextTxInfo = TxInfo
    { txInfoInputs = [TxInInfo (TxOutRef sampleTxId 0) sampleScriptTxOut, TxInInfo (TxOutRef sampleTxId 1) samplePubTxOut]
    , txInfoReferenceInputs = [TxInInfo (TxOutRef sampleTxId 2) sampleReferenceTxOut]
    , txInfoOutputs = [sampleScriptTxOut, samplePubTxOut]
    , txInfoFee = V2.Lovelace 0
    , txInfoMint = mempty
    , txInfoTxCerts = []
    , txInfoWdrl = unsafeFromList []
    , txInfoValidRange = always
    , txInfoSignatories = []
    , txInfoRedeemers = unsafeFromList [(Spending (TxOutRef sampleTxId 3), Redeemer (toBuiltinData dummyRedeemer))]
    , txInfoData = unsafeFromList []
    , txInfoId = fromString "00" :: TxId
    , txInfoVotes = unsafeFromList []
    , txInfoProposalProcedures = []
    , txInfoCurrentTreasuryAmount = Nothing :: Maybe V2.Lovelace
    , txInfoTreasuryDonation = Nothing      :: Maybe V2.Lovelace
    },
    scriptContextRedeemer = sampleRedeemer proof n,
    scriptContextScriptInfo = SpendingScript (TxOutRef sampleTxId 3) Nothing
  }

rollupScript :: SetupBytes -> ScriptContext -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
rollupScript paramsSetup ctx =
  getPlcNoAnn $ rollupCompiled paramsSetup
    `unsafeApplyCode` liftCodeDef (toBuiltinData ctx)

costsRollup :: SetupBytes -> ProofBytes -> [Int] -> [(Int, Integer, Integer)]
costsRollup s proof sizes = (\n -> (n, cpu n, mem n)) <$> sizes
  where
    costsCek = getCostsCek . rollupScript s . contextRollup proof
    cpu      = fst . costsCek
    mem      = snd . costsCek

testUpdateSizes :: [Int]
testUpdateSizes = [0, 1, 3, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50]

dataHeaders :: [String]
dataHeaders = ["Update length", "Exec Steps", "Exec Memory"]

dataOutputFile :: FilePath
dataOutputFile = "./data-analysis/rollupBench.csv"

main :: IO ()
main = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary

    let contract = EqualityCheckContract x ps targetValue

    createDirectoryIfMissing True "./data-analysis"
    createDirectoryIfMissing True "./data-analysis/test-data"

    BL.writeFile "./data-analysis/test-data/rollup-raw-contract-data.json" $ encode contract

    putStrLn $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n"

    let (setup, _, proof) = equalityCheckVerificationBytes x ps targetValue

    writeCSV dataOutputFile $ costsRollup setup proof testUpdateSizes
    printCSVWithHeaders dataOutputFile dataHeaders
    putStrLn ""
    putStrLn $ "Data exported to " ++ dataOutputFile
    putStrLn ""
