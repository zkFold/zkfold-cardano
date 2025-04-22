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
import           PlutusLedgerApi.V1.Value                 (lovelaceValue)
import qualified PlutusLedgerApi.V2                       as V2
import           PlutusLedgerApi.V3
import           PlutusLedgerApi.V3.MintValue             (MintValue (..))
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

import           ZkFold.Cardano.Examples.EqualityCheck    (equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.Utils            (savePlutus)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F       as F
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data      (InputBytes, ProofBytes (..), SetupBytes)
import           ZkFold.Cardano.UPLC.PlonkupVerifierToken (plonkupVerifierTokenCompiled)
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx    (plonkupVerifierTxCompiled)
import           ZkFold.Protocol.NonInteractiveProof      (NonInteractiveProof (..))


contextPlonkup :: ProofBytes -> ScriptContext
contextPlonkup redeemerProof = ScriptContext
  { scriptContextTxInfo = TxInfo
    { txInfoInputs = []                     :: [TxInInfo]
    , txInfoReferenceInputs = []            :: [TxInInfo]
    , txInfoOutputs = []                    :: [V2.TxOut]
    , txInfoFee = V2.Lovelace 0
    , txInfoMint = UnsafeMintValue $ unsafeFromList [(dummyCurrencySymbol, unsafeFromList [(dummyTokenName, 1)])]
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
dummyRedeemer = ProofBytes e e e e e e e e e e e e e 0 0 0 0 0 0 0 0 0 0 0 0 [F.F 0]
  where e = ""

dummyCredential :: Credential
dummyCredential = ScriptCredential . ScriptHash $ toBuiltin (fromString "deadbeef" :: BS.ByteString)

-- | An arbitrary address.
sampleAddress :: Address
sampleAddress = Address (PubKeyCredential . PubKeyHash $
                         (fromString "8b1dd80eb5d1da1afad0ed5a6be7eb9e46481a74621cb7d787caa3fc" :: BuiltinByteString)
                        ) Nothing

-- | 'change' = 'value' - 'fee'
sampleChange :: V2.Value
sampleChange = lovelaceValue $ V2.Lovelace 90000000

contextTx :: ProofBytes -> ScriptContext
contextTx redeemerProof = ScriptContext
  { scriptContextTxInfo = TxInfo
    { txInfoInputs = []                     :: [TxInInfo]
    , txInfoReferenceInputs = []            :: [TxInInfo]
    , txInfoOutputs = [ TxOut sampleAddress sampleChange NoOutputDatum Nothing ]
    , txInfoFee = V2.Lovelace 0             :: V2.Lovelace
    , txInfoMint = emptyMintValue           :: MintValue
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

plonkupVerifierTxScript :: SetupBytes -> ScriptContext -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
plonkupVerifierTxScript paramsSetup ctx =
    getPlcNoAnn $ plonkupVerifierTxCompiled paramsSetup
       `unsafeApplyCode` liftCodeDef (toBuiltinData ctx)

plonkupVerifierTokenScript :: SetupBytes -> ScriptContext -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
plonkupVerifierTokenScript paramsSetup ctx =
    getPlcNoAnn $ plonkupVerifierTokenCompiled paramsSetup
       `unsafeApplyCode` liftCodeDef (toBuiltinData ctx)

plonkupVerifierScript :: SetupBytes -> InputBytes -> ProofBytes -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
plonkupVerifierScript paramsSetup input proof =
    getPlcNoAnn $ plonkupVerifierCompiled paramsSetup
       `unsafeApplyCode` liftCodeDef (toBuiltinData input)
       `unsafeApplyCode` liftCodeDef (toBuiltinData proof)

printCostsPlonkupVerifierTx :: Handle -> SetupVerify PlonkupPlutus -> ScriptContext -> IO ()
printCostsPlonkupVerifierTx h s ctx = printSizeStatistics h NoSize (plonkupVerifierTxScript s ctx)

printCostsPlonkupVerifierToken :: Handle -> SetupVerify PlonkupPlutus -> ScriptContext -> IO ()
printCostsPlonkupVerifierToken h s ctx = printSizeStatistics h NoSize (plonkupVerifierTokenScript s ctx)

printCostsPlonkupVerifier :: Handle -> SetupVerify PlonkupPlutus -> Input PlonkupPlutus -> Proof PlonkupPlutus -> IO ()
printCostsPlonkupVerifier h s i p = printSizeStatistics h NoSize (plonkupVerifierScript s i p)

saveFlat ctx filePath code =
   BS.writeFile ("./" <> filePath <> ".flat") . flat . UnrestrictedProgram <$> getPlcNoAnn $ code
           `unsafeApplyCode` liftCodeDef (toBuiltinData ctx)

saveFlat2 input proof filePath code =
   BS.writeFile ("./" <> filePath <> ".flat") . flat . UnrestrictedProgram <$> getPlcNoAnn $ code
           `unsafeApplyCode` liftCodeDef (toBuiltinData input)
           `unsafeApplyCode` liftCodeDef (toBuiltinData proof)

untypedPlonkupVerifier :: SetupBytes -> BuiltinData -> BuiltinData -> BuiltinUnit
untypedPlonkupVerifier computation input' proof' =
    check
    ( verify @PlonkupPlutus
        computation
        [unsafeFromBuiltinData input']
        (unsafeFromBuiltinData proof')
    )

plonkupVerifierCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
plonkupVerifierCompiled computation =
    $$(compile [|| untypedPlonkupVerifier ||])
    `unsafeApplyCode` liftCodeDef computation


main :: IO ()
main = do
    x           <- generate arbitrary
    ps          <- generate arbitrary
    targetValue <- generate arbitrary

    let (setup, input, proof) = equalityCheckVerificationBytes x ps targetValue
        h = stdout

    createDirectoryIfMissing True "assets"

    savePlutus "assets/plonkupVerifierTx.plutus" $ plonkupVerifierTxCompiled setup
    savePlutus "assets/plonkupVerifierToken.plutus" $ plonkupVerifierTokenCompiled setup
    savePlutus "assets/plonkupVerifier.plutus" $ plonkupVerifierCompiled setup
    saveFlat (contextTx proof) "assets/plonkupVerifierTxScript" $ plonkupVerifierTxCompiled setup
    saveFlat (contextPlonkup proof) "assets/plonkupVerifierTokenScript"   $ plonkupVerifierTokenCompiled setup
    saveFlat2 input proof "assets/plonkupVerifierScript"  $ plonkupVerifierCompiled setup

    hPrintf h "\n\n"
    hPrintf h "Run \'plonkupVerifier\'\n\n"
    printHeader h
    printCostsPlonkupVerifier h setup input proof
    hPrintf h "\n\n"
    hPrintf h "\n\n"
    hPrintf h "Run \'plonkupVerifierToken\'\n\n"
    printHeader h
    printCostsPlonkupVerifierToken h setup $ contextPlonkup proof
    hPrintf h "\n\n"
    hPrintf h "\n\n"
    hPrintf h "Run \'plonkupVerifierTx\'\n\n"
    printHeader h
    printCostsPlonkupVerifierTx h setup $ contextTx proof
    hPrintf h "\n\n"

