{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import           Bench.Statistics                         (getCostsCek)
import           Bench.Utils                              (printCSVWithHeaders, writeCSV)
import           Data.Aeson                               (encode)
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Lazy                     as BL
import           Data.String                              (IsString (fromString))
import           PlutusCore                               (DefaultFun, DefaultUni)
import qualified PlutusLedgerApi.V2                       as V2
import           PlutusLedgerApi.V3
import           PlutusTx                                 (getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                         (($), (.))
import           Prelude                                  hiding (Bool, Eq (..), Fractional (..), Num (..), length, ($),
                                                           (.))
import           System.Directory                         (createDirectoryIfMissing)
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)
import qualified UntypedPlutusCore                        as UPLC

import           ZkFold.Cardano.Examples.EqualityCheck    (equalityCheckVerificationBytes)
import           ZkFold.Cardano.Plonk.OffChain            (EqualityCheckContract (..))
import           ZkFold.Cardano.Plonk.OnChain             (ProofBytes (..), SetupBytes)
import           ZkFold.Cardano.Plonk.OnChain.BLS12_381.F (F (..))
import           ZkFold.Cardano.Scripts.Rollup            (RollupRedeemer (..))
import           ZkFold.Cardano.UPLC                      (rollupCompiled)


sampleRedeemer :: ProofBytes -> Int -> Redeemer
sampleRedeemer proof n =
  let sampleAddress = Address (PubKeyCredential . PubKeyHash $
                                toBuiltin (fromString "8b1dd80eb5d1da1afad0ed5a6be7eb9e46481a74621cb7d787caa3fc" :: BS.ByteString)
                              ) Nothing
      sampleValue   = V2.singleton V2.adaSymbol V2.adaToken 100000000
      sampleState   = F 25154496976141666116585768883114414650575212708960519991881605349447581737748
  in Redeemer . toBuiltinData $ RollupRedeemer
       { rrProof   = proof
       , rrAddress = sampleAddress
       , rrValue   = sampleValue
       , rrState   = sampleState
       , rrUpdate  = replicate n sampleState
       }

dummyCurrencySymbol :: CurrencySymbol
dummyCurrencySymbol = CurrencySymbol $ toBuiltin (fromString "264ac730a6d3dacd0be8f9948e161aa151fd49d5e48203c31b2ae5eb" :: BS.ByteString)

dummyTokenName :: TokenName
dummyTokenName = TokenName $ toBuiltin (fromString "34ad74db78700c335968ca0898f2953adba88f368efa0541b98897e2668090bd" :: BS.ByteString)

dummyRedeemer :: ProofBytes
dummyRedeemer = ProofBytes e e e e e e e e e 0 0 0 0 0 0 (F 0)
  where e = ""

contextRollup :: ProofBytes -> Int -> ScriptContext
contextRollup proof n = ScriptContext
  { scriptContextTxInfo = TxInfo
    { txInfoInputs = []                     :: [TxInInfo]
    , txInfoReferenceInputs = []            :: [TxInInfo]
    , txInfoOutputs = []                    :: [V2.TxOut]
    , txInfoFee = V2.Lovelace 0
    , txInfoMint = Value $ unsafeFromList [(dummyCurrencySymbol, unsafeFromList [(dummyTokenName, 1)])]
    , txInfoTxCerts = []
    , txInfoWdrl = unsafeFromList []
    , txInfoValidRange = always
    , txInfoSignatories = []
    , txInfoRedeemers = unsafeFromList [(Minting dummyCurrencySymbol, Redeemer (toBuiltinData dummyRedeemer))]
    , txInfoData = unsafeFromList []
    , txInfoId = fromString "00" :: TxId
    , txInfoVotes = unsafeFromList []
    , txInfoProposalProcedures = []
    , txInfoCurrentTreasuryAmount = Nothing :: Maybe V2.Lovelace
    , txInfoTreasuryDonation = Nothing      :: Maybe V2.Lovelace
    },
    scriptContextRedeemer = sampleRedeemer proof n,
    scriptContextScriptInfo = MintingScript dummyCurrencySymbol
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
