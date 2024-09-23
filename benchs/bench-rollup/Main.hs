{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import           Bench.Statistics                         (getCostsCek)
-- import           Cardano.Api                              (File (..), IsPlutusScriptLanguage, PlutusScript,
--                                                            PlutusScriptV3, writeFileTextEnvelope)
-- import           Cardano.Api.Shelley                      (PlutusScript (..))
-- import           Control.Monad                            (void)
import qualified Data.ByteString                          as BS
import           Data.Maybe                               (fromJust)
import           Data.String                              (IsString (fromString))
import           Data.Text                                (pack)
-- import           Flat                                     (flat)
-- import           Flat.Types                               ()
import           PlutusCore                               (DefaultFun, DefaultUni)
import qualified PlutusLedgerApi.V2                       as V2
import           PlutusLedgerApi.V3
import           PlutusTx                                 (getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                         (($), (.))
import           Prelude                                  hiding (Bool, Eq (..), Fractional (..), Num (..), length, ($),
                                                           (.))
-- import           System.Directory                         (createDirectoryIfMissing)
-- import           System.IO                                (Handle, stdout)
-- import           System.IO                                (writeFile)
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)
import           Text.Hex                                 (decodeHex)
-- import           Text.Printf                              (hPrintf)
import qualified UntypedPlutusCore                        as UPLC
-- import           UntypedPlutusCore                        (UnrestrictedProgram (..))

-- import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Examples.EqualityCheck    (equalityCheckVerificationBytes)
-- import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (ProofBytes (..), SetupBytes)
import           ZkFold.Cardano.Plonk.OnChain.BLS12_381.F (F (..))
import           ZkFold.Cardano.Scripts.Rollup            (RollupRedeemer (..))
import           ZkFold.Cardano.UPLC                      (rollupCompiled)


writeCSV :: FilePath -> [(Int, Integer, Integer)] -> IO ()
writeCSV path tuples = do
    let csvLines = map (\(n, x, y) -> show n ++ "," ++ show x ++ "," ++ show y) tuples
    writeFile path (unlines csvLines)

hexConvert :: String -> BuiltinByteString
hexConvert = toBuiltin . fromJust . decodeHex . pack

sampleRedeemer :: ProofBytes -> Int -> Redeemer
sampleRedeemer proof n =
  let sampleAddress = Address
        (PubKeyCredential . PubKeyHash . hexConvert $ "8b1dd80eb5d1da1afad0ed5a6be7eb9e46481a74621cb7d787caa3fc")
        Nothing
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

-- dummyCredential :: Credential
-- dummyCredential = ScriptCredential . ScriptHash $ toBuiltin (fromString "deadbeef" :: BS.ByteString)

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

main :: IO ()
main = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary

    let (setup, _, proof) = equalityCheckVerificationBytes x ps targetValue
    writeCSV "./rollupBench.csv" $ costsRollup setup proof testUpdateSizes
