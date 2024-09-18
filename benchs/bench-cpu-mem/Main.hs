{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where


import           Bench.Statistics                         (TestSize (..), printHeader, printSizeStatistics)
import           Cardano.Api                              (File (..), IsPlutusScriptLanguage, PlutusScript,
                                                           PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Shelley                      (PlutusScript (..))
import           Control.Monad                            (void)
import qualified Data.ByteString                          as BS
import qualified Data.Maybe                               as Haskell
import           Data.String                              (IsString (fromString))
import           Flat                                     (flat)
import           Flat.Types                               ()
import           PlutusCore                               (DefaultFun, DefaultUni)
import qualified PlutusLedgerApi.V2                       as V2
import           PlutusLedgerApi.V3
import           PlutusTx                                 (CompiledCode, getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                         (($), (.))
import           Prelude                                  hiding (Bool, Eq (..), Fractional (..), Num (..), length, ($),
                                                           (.))
import           System.Directory                         (createDirectoryIfMissing)
import           System.IO                                (Handle, stdout)
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)
import           Text.Printf                              (hPrintf)
import qualified UntypedPlutusCore                        as UPLC
import           UntypedPlutusCore                        (UnrestrictedProgram (..))

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Examples.EqualityCheck    (equalityCheckVerificationBytes)
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (InputBytes, ProofBytes (..), SetupBytes)
import qualified ZkFold.Cardano.Plonk.OnChain.BLS12_381.F as F
import           ZkFold.Cardano.UPLC                      (plonkVerifierCompiled, symbolicVerifierCompiled,
                                                           verifyPlonkCompiled)


contextPlonk :: ScriptContext
contextPlonk = ScriptContext
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
    , txInfoCurrentTreasuryAmount = Nothing :: Haskell.Maybe V2.Lovelace
    , txInfoTreasuryDonation = Nothing      :: Haskell.Maybe V2.Lovelace
    },
    scriptContextRedeemer = Redeemer (toBuiltinData dummyRedeemer),
    scriptContextScriptInfo = MintingScript dummyCurrencySymbol
  }

dummyCurrencySymbol :: CurrencySymbol
dummyCurrencySymbol = CurrencySymbol $ toBuiltin (fromString "264ac730a6d3dacd0be8f9948e161aa151fd49d5e48203c31b2ae5eb" :: BS.ByteString)

dummyTokenName :: TokenName
dummyTokenName = TokenName $ toBuiltin (fromString "34ad74db78700c335968ca0898f2953adba88f368efa0541b98897e2668090bd" :: BS.ByteString)

dummyRedeemer :: ProofBytes
dummyRedeemer = ProofBytes e e e e e e e e e 0 0 0 0 0 0 (F.F 0)
  where e = ""

{-
contextSymbolic :: ScriptContext
contextSymbolic = ScriptContext
  { scriptContextTxInfo = TxInfo
    { txInfoInputs = []                     :: [TxInInfo]
    , txInfoReferenceInputs = []            :: [TxInInfo]
    , txInfoOutputs = []                    :: [V2.TxOut]
    , txInfoFee = _                         :: V2.Lovelace
    , txInfoMint = _                        :: V2.Value
    , txInfoTxCerts = []                    :: [TxCert]
    , txInfoWdrl = _                        :: Map V2.Credential V2.Lovelace
    , txInfoValidRange = always             :: V2.POSIXTimeRange
    , txInfoSignatories = []                :: [V2.PubKeyHash]
    , txInfoRedeemers = _                   :: Map ScriptPurpose V2.Redeemer
    , txInfoData = _                        :: Map V2.DatumHash V2.Datum
    , txInfoId = _                          :: V3.TxId
    , txInfoVotes = _                       :: Map Voter (Map GovernanceActionId Vote)
    , txInfoProposalProcedures = []         :: [ProposalProcedure]
    , txInfoCurrentTreasuryAmount = Nothing :: Haskell.Maybe V2.Lovelace
    , txInfoTreasuryDonation = Nothing      :: Haskell.Maybe V2.Lovelace
    },
    scriptContextRedeemer = Redeemer _,
    scriptContextScriptInfo = RewardingScript _
  }
-}

symbolicVerifierScript :: SetupBytes -> ProofBytes -> ScriptContext -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
symbolicVerifierScript paramsSetup redeemerProof ctx =
    getPlcNoAnn $ symbolicVerifierCompiled paramsSetup
       `unsafeApplyCode` liftCodeDef (toBuiltinData redeemerProof)
       `unsafeApplyCode` liftCodeDef (toBuiltinData ctx)

plonkVerifierScript :: SetupBytes -> ProofBytes -> ScriptContext -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
plonkVerifierScript paramsSetup redeemerProof ctx =
    getPlcNoAnn $ symbolicVerifierCompiled paramsSetup
       `unsafeApplyCode` liftCodeDef (toBuiltinData redeemerProof)
       `unsafeApplyCode` liftCodeDef (toBuiltinData ctx)

verifyPlonkScript :: SetupBytes -> InputBytes -> ProofBytes -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
verifyPlonkScript paramsSetup input redeemerProof =
    getPlcNoAnn $ verifyPlonkCompiled paramsSetup
       `unsafeApplyCode` liftCodeDef (toBuiltinData input)
       `unsafeApplyCode` liftCodeDef (toBuiltinData redeemerProof)

printCostsSymbolicVerifier :: Handle -> SetupVerify PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> IO ()
printCostsSymbolicVerifier h s p ctx = printSizeStatistics h NoSize (symbolicVerifierScript s p ctx)

printCostsPlonkVerifier :: Handle -> SetupVerify PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> IO ()
printCostsPlonkVerifier h s p ctx = printSizeStatistics h NoSize (plonkVerifierScript s p ctx)

printCostsVerifyPlonk :: Handle -> SetupVerify PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> IO ()
printCostsVerifyPlonk h s i p = printSizeStatistics h NoSize (verifyPlonkScript s i p)

saveFlat redeemer filePath code =
   BS.writeFile ("./" <> filePath <> ".flat") . flat . UnrestrictedProgram <$> getPlcNoAnn $ code
           `unsafeApplyCode` liftCodeDef (toBuiltinData redeemer)

writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . serialiseCompiledCode

main :: IO ()
main = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary

    let (setup, input, proof) = equalityCheckVerificationBytes x ps targetValue
        h = stdout
        redeemer = (setup, input, proof)

    createDirectoryIfMissing True "assets"

    savePlutus "assets/symbolicVerifier" $ symbolicVerifierCompiled setup
    savePlutus "assets/plonkVerifier"    $ plonkVerifierCompiled setup
    savePlutus "assets/verifyPlonk"      $ verifyPlonkCompiled setup
    saveFlat proof "assets/plonkSymbolicVerifier" $ symbolicVerifierCompiled setup
    saveFlat proof "assets/plonkVerifierScript"   $ plonkVerifierCompiled setup
    saveFlat redeemer "assets/verifyPlonkScript"  $ verifyPlonkCompiled setup

    hPrintf h "\n\n"
    hPrintf h "Run plonk verify\n\n"
    printHeader h
    printCostsVerifyPlonk h setup input proof
    hPrintf h "\n\n"
    hPrintf h "\n\n"
    hPrintf h "Run plonk verifier\n\n"
    printHeader h
    printCostsPlonkVerifier h setup proof contextPlonk
    -- hPrintf h "\n\n"
    -- hPrintf h "\n\n"
    -- hPrintf h "Run symbolic plonk verifier\n\n"
    -- printHeader h
    -- printCostsSymbolicVerifier h setup proof contextSymbolic
    hPrintf h "\n\n"

