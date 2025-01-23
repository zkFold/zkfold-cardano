{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where


import           Bench.Statistics                         (TestSize (..), printHeader, printSizeStatistics)
import qualified Data.ByteString                          as BS
import qualified Data.Maybe                               as Haskell
import           Data.String                              (IsString (fromString))
import           Flat                                     (flat)
import           Flat.Types                               ()
import           PlutusCore                               (DefaultFun, DefaultUni)
import qualified PlutusLedgerApi.V2                       as V2
import           PlutusLedgerApi.V3
import           PlutusTx                                 (CompiledCode, compile, getPlcNoAnn, liftCodeDef,
                                                           unsafeApplyCode)
import           PlutusTx.Prelude                         (BuiltinUnit, check, ($), (.))
import           Prelude                                  hiding (Bool, Eq (..), Fractional (..), Num (..), length, ($),
                                                           (.))
import           System.Directory                         (createDirectoryIfMissing)
import           System.IO                                (Handle, stdout)
import           Test.QuickCheck.Arbitrary                (Arbitrary (..))
import           Test.QuickCheck.Gen                      (generate)
import           Text.Printf                              (hPrintf)
import qualified UntypedPlutusCore                        as UPLC
import           UntypedPlutusCore                        (UnrestrictedProgram (..))

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Cardano.Examples.EqualityCheck    (equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils            (savePlutus)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F       as F
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data      (InputBytes, ProofBytes (..), SetupBytes)
import           ZkFold.Cardano.UPLC.PlonkVerifierToken   (plonkVerifierTokenCompiled)
import           ZkFold.Cardano.UPLC.PlonkVerifierTx      (plonkVerifierTxCompiled)


contextPlonk :: ProofBytes -> ScriptContext
contextPlonk redeemerProof = ScriptContext
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
    scriptContextRedeemer = Redeemer (toBuiltinData redeemerProof),
    scriptContextScriptInfo = MintingScript dummyCurrencySymbol
  }

dummyCurrencySymbol :: CurrencySymbol
dummyCurrencySymbol = CurrencySymbol $ toBuiltin (fromString "264ac730a6d3dacd0be8f9948e161aa151fd49d5e48203c31b2ae5eb" :: BS.ByteString)

dummyTokenName :: TokenName
dummyTokenName = TokenName $ toBuiltin (fromString "34ad74db78700c335968ca0898f2953adba88f368efa0541b98897e2668090bd" :: BS.ByteString)

dummyRedeemer :: ProofBytes
dummyRedeemer = ProofBytes e e e e e e e e e e e e e 0 0 0 0 0 0 0 0 0 0 0 0 (F.F 0)
  where e = ""

dummyCredential :: Credential
dummyCredential = ScriptCredential . ScriptHash $ toBuiltin (fromString "deadbeef" :: BS.ByteString)

contextTx :: ProofBytes -> ScriptContext
contextTx redeemerProof = ScriptContext
  { scriptContextTxInfo = TxInfo
    { txInfoInputs = []                     :: [TxInInfo]
    , txInfoReferenceInputs = []            :: [TxInInfo]
    , txInfoOutputs = []                    :: [V2.TxOut]
    , txInfoFee = V2.Lovelace 0             :: V2.Lovelace
    , txInfoMint = mempty                   :: V2.Value
    , txInfoTxCerts = []                    :: [TxCert]
    , txInfoWdrl = unsafeFromList []        :: Map V2.Credential V2.Lovelace
    , txInfoValidRange = always             :: V2.POSIXTimeRange
    , txInfoSignatories = []                :: [V2.PubKeyHash]
    , txInfoRedeemers = unsafeFromList []   :: Map ScriptPurpose V2.Redeemer
    , txInfoData = unsafeFromList []        :: Map V2.DatumHash V2.Datum
    , txInfoId = fromString "00"            :: TxId
    , txInfoVotes = unsafeFromList []       :: Map Voter (Map GovernanceActionId Vote)
    , txInfoProposalProcedures = []         :: [ProposalProcedure]
    , txInfoCurrentTreasuryAmount = Nothing :: Haskell.Maybe V2.Lovelace
    , txInfoTreasuryDonation = Nothing      :: Haskell.Maybe V2.Lovelace
    },
    scriptContextRedeemer = Redeemer (toBuiltinData redeemerProof),
    scriptContextScriptInfo = RewardingScript dummyCredential
  }

plonkVerifierTxScript :: SetupBytes -> ScriptContext -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
plonkVerifierTxScript paramsSetup ctx =
    getPlcNoAnn $ plonkVerifierTxCompiled paramsSetup
       `unsafeApplyCode` liftCodeDef (toBuiltinData ctx)

plonkVerifierTokenScript :: SetupBytes -> ScriptContext -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
plonkVerifierTokenScript paramsSetup ctx =
    getPlcNoAnn $ plonkVerifierTokenCompiled paramsSetup
       `unsafeApplyCode` liftCodeDef (toBuiltinData ctx)

plonkVerifierScript :: SetupBytes -> InputBytes -> ProofBytes -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
plonkVerifierScript paramsSetup input proof =
    getPlcNoAnn $ plonkVerifierCompiled paramsSetup
       `unsafeApplyCode` liftCodeDef (toBuiltinData input)
       `unsafeApplyCode` liftCodeDef (toBuiltinData proof)

printCostsPlonkVerifierTx :: Handle -> SetupVerify PlonkupPlutus -> ScriptContext -> IO ()
printCostsPlonkVerifierTx h s ctx = printSizeStatistics h NoSize (plonkVerifierTxScript s ctx)

printCostsPlonkVerifierToken :: Handle -> SetupVerify PlonkupPlutus -> ScriptContext -> IO ()
printCostsPlonkVerifierToken h s ctx = printSizeStatistics h NoSize (plonkVerifierTokenScript s ctx)

printCostsPlonkVerifier :: Handle -> SetupVerify PlonkupPlutus -> Input PlonkupPlutus -> Proof PlonkupPlutus -> IO ()
printCostsPlonkVerifier h s i p = printSizeStatistics h NoSize (plonkVerifierScript s i p)

saveFlat ctx filePath code =
   BS.writeFile ("./" <> filePath <> ".flat") . flat . UnrestrictedProgram <$> getPlcNoAnn $ code
           `unsafeApplyCode` liftCodeDef (toBuiltinData ctx)

saveFlat2 input proof filePath code =
   BS.writeFile ("./" <> filePath <> ".flat") . flat . UnrestrictedProgram <$> getPlcNoAnn $ code
           `unsafeApplyCode` liftCodeDef (toBuiltinData input)
           `unsafeApplyCode` liftCodeDef (toBuiltinData proof)

untypedPlonkVerifier :: SetupBytes -> BuiltinData -> BuiltinData -> BuiltinUnit
untypedPlonkVerifier computation input' proof' =
    check
    ( verify @PlonkupPlutus @HaskellCore
        computation
        (unsafeFromBuiltinData input')
        (unsafeFromBuiltinData proof')
    )


plonkVerifierCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
plonkVerifierCompiled computation =
    $$(compile [|| untypedPlonkVerifier ||])
    `unsafeApplyCode` liftCodeDef computation


main :: IO ()
main = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary

    let (setup, input, proof) = equalityCheckVerificationBytes x ps targetValue
        h = stdout

    createDirectoryIfMissing True "assets"

    savePlutus "assets/plonkVerifierTx" $ plonkVerifierTxCompiled setup
    savePlutus "assets/plonkVerifierToken"    $ plonkVerifierTokenCompiled setup
    savePlutus "assets/plonkVerifier"      $ plonkVerifierCompiled setup
    saveFlat (contextTx proof) "assets/plonkPlonkVerifierTx" $ plonkVerifierTxCompiled setup
    saveFlat (contextPlonk proof) "assets/plonkVerifierTokenScript"   $ plonkVerifierTokenCompiled setup
    saveFlat2 input proof "assets/plonkVerifierScript"  $ plonkVerifierCompiled setup

    hPrintf h "\n\n"
    hPrintf h "Run \'plonkVerifier\'\n\n"
    printHeader h
    printCostsPlonkVerifier h setup input proof
    hPrintf h "\n\n"
    hPrintf h "\n\n"
    hPrintf h "Run \'plonkVerifierToken\'\n\n"
    printHeader h
    printCostsPlonkVerifierToken h setup $ contextPlonk proof
    hPrintf h "\n\n"
    hPrintf h "\n\n"
    hPrintf h "Run \'plonkVerifierTx\'\n\n"
    printHeader h
    printCostsPlonkVerifierTx h setup $ contextTx proof
    hPrintf h "\n\n"

