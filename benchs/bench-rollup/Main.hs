{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import           GHC.ByteOrder                               (ByteOrder (..))
import           Data.Aeson                                  (encode)
import qualified Data.ByteString.Lazy                        as BL
import           Data.String                                 (IsString (fromString))
import           PlutusCore                                  (DefaultFun, DefaultUni)
import           PlutusLedgerApi.V1.Value                    (lovelaceValue)
import qualified PlutusLedgerApi.V2                          as V2
import           PlutusLedgerApi.V3
import           PlutusTx                                    (getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Builtins                           (byteStringToInteger, mkI)
import           PlutusTx.Prelude                            (($), (.))
import           Prelude                                     hiding (Bool, Eq(..), Fractional(..), length, ($), (.))
import           System.Directory                            (createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath                             (takeFileName, (</>))
import           Test.QuickCheck.Arbitrary                   (Arbitrary(..))
import           Test.QuickCheck.Gen                         (generate)
import qualified UntypedPlutusCore                           as UPLC

import           Bench.Statistics                            (getCostsCek)
import           Bench.Utils                                 (intToByteString32, printCSVWithHeaders, writeCSV)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)
import           ZkFold.Cardano.Examples.IdentityCircuit     (stateCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E                 (IdentityCircuitContract(..))
import           ZkFold.Cardano.OnChain.BLS12_381.F          (toF)
import           ZkFold.Cardano.OnChain.Plonk.Data           (ProofBytes(..), SetupBytes(..))
import           ZkFold.Cardano.OnChain.Utils                (dataToBlake)
import           ZkFold.Cardano.UPLC                         (rollupCompiled)
import           ZkFold.Cardano.UPLC.Rollup                  (RollupRedeemer(..), RollupSetup(..))


--------------- SETUP  ----------------

-- | Construct "update" of length n.
updateOfLength :: Int -> [BuiltinByteString]
updateOfLength n = [intToByteString32 i | i <- [0 .. n - 1]]

-- | An arbitrary currency symbol.
sampleDataCurrency :: CurrencySymbol
sampleDataCurrency = CurrencySymbol (fromString "279c909f348e533da5808898f87f9a14bb2c3dfbbacccd631d927a3f" :: BuiltinByteString)

-- | An arbitrary thread token.
sampleThreadValue :: Value
sampleThreadValue = singleton threadCurrency threadTokenName 1
  where
    threadCurrency  = CurrencySymbol (fromString "17ac801d2ce81747e038d69bc0ccc861dcaad0d10750df4a3897b66d" :: BuiltinByteString)
    threadTokenName = TokenName (fromString "7a6b466f6c64" :: BuiltinByteString)  -- token name: "zkFold"

-- | Value containing "update" token names.
updateValue :: Int -> Value
updateValue n = Value $ unsafeFromList [(sampleDataCurrency, unsafeFromList . map (\bs -> (TokenName bs, 1)) $ updateOfLength n)]

-- | Rollup redeemer for an "update" of length 'n'.
rollupRedeemer :: ProofBytes -> Int -> Redeemer
rollupRedeemer proof n = Redeemer . toBuiltinData . UpdateRollup proof $ updateOfLength n

unitDatum :: OutputDatum
unitDatum = OutputDatum . Datum . toBuiltin . toData $ ()

-- | An arbitrary address.
sampleAddress :: Address
sampleAddress = Address (PubKeyCredential . PubKeyHash $
                         (fromString "8b1dd80eb5d1da1afad0ed5a6be7eb9e46481a74621cb7d787caa3fc" :: BuiltinByteString)
                        ) Nothing

-- | An arbitrary value.
sampleValue :: V2.Value
sampleValue = lovelaceValue $ V2.Lovelace 100000000

-- | An arbitrary fee.
sampleFee :: V2.Lovelace
sampleFee = V2.Lovelace 10000000

-- | 'change' = 'value' - 'fee'
sampleChange :: V2.Value
sampleChange = lovelaceValue $ V2.Lovelace 90000000

-- | An arbitary integer.
sampleState :: Integer
sampleState = 25154496976141666116585768883114414650575212708960519991881605349447581737748

-- | An arbitrary 'TxId'.
sampleTxId :: TxId
sampleTxId = TxId (fromString "25923f589a26311e87fb37bb41cb1dadf9a90166775f9f3b303cfe24e4fb95f8" :: BuiltinByteString)

-- | Reference TxOut carrying an "update" value of length 'n'.
updateRefTxOut :: Int -> TxOut
updateRefTxOut n = TxOut sampleAddress (updateValue n) unitDatum Nothing

-- | TxOut carrying the "state" datum.
stateTxOut :: Integer -> TxOut
stateTxOut state = TxOut sampleAddress sampleThreadValue (OutputDatum . Datum . mkI $ state) Nothing

-- | Sample pub key output
samplePubTxOut :: Value -> TxOut
samplePubTxOut v = TxOut sampleAddress v NoOutputDatum Nothing

-- | An arbitrary datum hash.
sampleDatumHash :: OutputDatum
sampleDatumHash = OutputDatumHash $ DatumHash (fromString "9f5e3c6d1d8f98d45b62a7d113e1b9f5e3a7d8f32a9f7c6d4b8f9a3c5d7e8f9b" :: BuiltinByteString)

-- | TxOut carrying a bridge output.
sampleBridgeTxOut :: TxOut
sampleBridgeTxOut = TxOut sampleAddress sampleValue sampleDatumHash Nothing

-- | Updated state taking into account 'sampleState' and "update" list.
sampleNewState :: Int -> Integer
sampleNewState n = byteStringToInteger BigEndian $ dataToBlake (toF sampleState, updateOfLength n, [sampleBridgeTxOut], sampleValue)

-- | The 'ScriptContext'.
contextRollup :: ProofBytes -> Int -> ScriptContext
contextRollup proof n = ScriptContext
  { scriptContextTxInfo = TxInfo
    { txInfoInputs = [TxInInfo (TxOutRef sampleTxId 0) (stateTxOut sampleState), TxInInfo (TxOutRef sampleTxId 1) (samplePubTxOut sampleValue)]
    , txInfoReferenceInputs = [TxInInfo (TxOutRef sampleTxId 2) (updateRefTxOut n)]
    , txInfoOutputs =
        [ stateTxOut (sampleNewState n)
        , samplePubTxOut sampleValue
        , samplePubTxOut sampleChange
        , sampleBridgeTxOut
        ]
    , txInfoFee = sampleFee
    , txInfoMint = mempty
    , txInfoTxCerts = []
    , txInfoWdrl = unsafeFromList []
    , txInfoValidRange = always
    , txInfoSignatories = []
    , txInfoRedeemers = unsafeFromList [(Spending (TxOutRef sampleTxId 0), rollupRedeemer proof n)]
    , txInfoData = unsafeFromList []
    , txInfoId = fromString "00" :: TxId
    , txInfoVotes = unsafeFromList []
    , txInfoProposalProcedures = []
    , txInfoCurrentTreasuryAmount = Nothing :: Maybe V2.Lovelace
    , txInfoTreasuryDonation = Nothing      :: Maybe V2.Lovelace
    },
    scriptContextRedeemer = rollupRedeemer proof n,
    scriptContextScriptInfo = SpendingScript (TxOutRef sampleTxId 0) Nothing
  }

rollupSetup :: SetupBytes -> RollupSetup
rollupSetup setup = RollupSetup
  { rsLedgerRules  = setup
  , rsDataCurrency = sampleDataCurrency
  , rsThreadValue  = sampleThreadValue
  , rsFeeAddress   = sampleAddress
  }


--------------- BENCHMARK ROLLUP SCRIPT  ----------------

rollupScript :: RollupSetup -> ScriptContext -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
rollupScript setup ctx =
  getPlcNoAnn $ rollupCompiled setup
    `unsafeApplyCode` liftCodeDef (toBuiltinData ctx)

costRollup :: Fr -> PlonkupProverSecret BLS12_381_G1 -> Int -> (Int, Integer, Integer)
costRollup x ps n = (n, cpu, mem)
  where
    (setup, _, proof) = stateCheckVerificationBytes x ps . toF $ sampleNewState n
    costsCek = getCostsCek . rollupScript (rollupSetup setup) $ contextRollup proof n
    cpu      = fst costsCek
    mem      = snd costsCek

testUpdateSizes :: [Int]
testUpdateSizes = [0, 50 .. 1000]

dataHeaders :: [String]
dataHeaders = ["Update length", "Exec Steps", "Exec Memory"]


--------------- MAIN  ----------------

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path    = case takeFileName currentDir of
        "data-analysis" -> "."
        "bench-rollup"  -> "data-analysis"
        "benchs"        -> "bench-rollup" </> "data-analysis"
        _               -> "benchs" </> "bench-rollup" </> "data-analysis"

  let dataOutputFile = path </> "rollupBench.csv"

  x  <- generate arbitrary
  ps <- generate arbitrary

  let contract = IdentityCircuitContract x ps

  createDirectoryIfMissing True $ path
  createDirectoryIfMissing True $ path </> "test-data"

  BL.writeFile (path </> "test-data" </> "rollup-raw-contract-data.json") (encode contract)

  putStrLn $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n"

  writeCSV dataOutputFile $ costRollup x ps <$> testUpdateSizes
  printCSVWithHeaders dataOutputFile dataHeaders

  putStrLn ""
  putStrLn $ "Data exported to " ++ dataOutputFile
  putStrLn ""
