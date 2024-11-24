module Main where

import           Cardano.Api                           (IsPlutusScriptLanguage, PlutusScriptV3, writeFileTextEnvelope)
import           Cardano.Api.Shelley                   (File (..), PlutusScript (..))
import           Control.Monad                         (void)
import           Data.Aeson                            (encode)
import qualified Data.ByteString.Lazy                  as BL
import qualified PlutusLedgerApi.V3                    as PlutusV3
import           PlutusTx                              (CompiledCode)
import           Prelude                               (Bool (..), FilePath, IO, Maybe (..), Show (..), putStr, ($),
                                                        (++), (.))
import           System.Directory                      (createDirectoryIfMissing)
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)

import           ZkFold.Cardano.Examples.EqualityCheck (equalityCheckVerificationBytes)
import           ZkFold.Cardano.OffChain.E2E           (EqualityCheckContract (..))
import           ZkFold.Cardano.UPLC                   (forwardingRewardCompiled, plonkVerifierTxCompiled)

{-
import           Cardano.Api                                 hiding (TxId)
import qualified Cardano.Api                                 as C
import           Cardano.Api.Shelley                         (PlutusScriptOrReferenceInput (..), fromPlutusData, scriptDataToJsonDetailedSchema, shelleyPayAddrToPlutusPubKHash)
import           Cardano.Binary                              (serialize')
import           Cardano.CLI.Environment                     (getEnvCli)
import           Cardano.CLI.EraBased.Commands               (AnyEraCommand (..), Cmds (..))
import           Cardano.CLI.EraBased.Commands.Transaction   (TransactionBuildCmdArgs (..), TransactionCmds (..))
import           Cardano.CLI.EraBased.Run.Transaction        (toTxOutInAnyEra)
import           Cardano.CLI.Options                         (opts, pref)
import           Cardano.CLI.OS.Posix                        (groupModes, otherModes, setFileCreationMask, unionFileModes)
import           Cardano.CLI.Read                            (readScriptWitnessFiles)
import           Cardano.CLI.Run                             (ClientCommand (..))
import           Cardano.CLI.TopHandler                      (toplevelExceptionHandler)
import           Cardano.CLI.Types.Common                    (TxOutChangeAddress (..))
import qualified Cardano.Crypto.Init                         as Crypto
import           Codec.Serialise                             (Serialise (encode))
import           Data.Aeson                                  (decode)
import qualified Data.Aeson                                  as Aeson
import           Data.ByteString                             as BS (writeFile)
import qualified Data.ByteString.Base16                      as B16
import qualified Data.ByteString.Lazy                        as BL
import           Data.Either                                 (partitionEithers)
import           Data.Map                                    (fromList, keys)
import qualified Data.Map.Strict                             as Map
import           Data.String                                 (IsString (..))
import           GHC.ByteOrder                               (ByteOrder (..))
import qualified GHC.IO.Encoding                             as GHC
import qualified Options.Applicative                         as Opt
import           PlutusLedgerApi.V3                          (BuiltinByteString, CurrencySymbol (..), POSIXTimeRange, TokenName (..), adaSymbol, adaToken, always)
import qualified PlutusLedgerApi.V3                          as V3
import           PlutusTx                                    (ToData (..))
import qualified PlutusTx.AssocMap                           as AssocMap
import           PlutusTx.Builtins                           (blake2b_224, byteStringToInteger, serialiseData)
import           PlutusTx.Prelude                            (takeByteString)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)

import           ZkFold.Base.Algebra.Basic.Class             (AdditiveGroup (..), FromConstant (..))
import           ZkFold.Base.Algebra.Basic.Field             (toZp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar, Fr)
import           ZkFold.Base.Protocol.ARK.Plonk              (F, Plonk (..), PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk.OffChain               (EqualityCheckContract (..), PlonkN, RowContractJSON, mkInput, mkProof, mkSetup, toContract)
import           ZkFold.Cardano.ScriptsVerifier              (RedeemerSymbolic (..))
import           ZkFold.Symbolic.Cardano.Types               (TxId (..))
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit  (applyArgs)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Types                       (Symbolic)
import           ZkFold.Base.Data.Vector                     (Vector(..))

dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . V3.toData

fromValueNestedRep :: ValueNestedRep -> AssocMap.Map CurrencySymbol (AssocMap.Map TokenName Integer)
fromValueNestedRep (ValueNestedRep vbs) = AssocMap.safeFromList $ fmap fromNestedBundle vbs
  where
    fromPolicy :: PolicyId -> CurrencySymbol -- It is empty for `Ada`, 28 bytes for `MintingPolicyHash`.
    fromPolicy (PolicyId scripthash) = CurrencySymbol $ fromString @BuiltinByteString $ show scripthash

    fromTokenName :: AssetName -> TokenName -- Should be no longer than 32 bytes, empty for Ada.
    fromTokenName (AssetName byteString) = TokenName $ fromString @BuiltinByteString $ show byteString

    fromNestedBundle :: ValueNestedBundle -> (CurrencySymbol, AssocMap.Map TokenName Integer)
    fromNestedBundle (ValueNestedBundleAda (Quantity coin)) = (adaSymbol, AssocMap.singleton @TokenName @Integer adaToken coin)
    fromNestedBundle (ValueNestedBundle policyId map') = (fromPolicy policyId, map'')
      where map'' = AssocMap.safeFromList $ (\(a, Quantity b) -> (fromTokenName a, b)) <$> Map.toList map'

getOutBob :: TxOut CtxTx ConwayEra -> V3.TxOut
getOutBob txout = do
  let pubkey = case txout of
        (TxOut (AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) a) _ _ _) -> case shelleyPayAddrToPlutusPubKHash a of
          Just b -> b
  let v = case txout of
        (TxOut _ a _ _) -> valueToNestedRep $ txOutValueToValue a
  V3.TxOut {
  txOutAddress = V3.Address {
    addressCredential = V3.PubKeyCredential pubkey,
    addressStakingCredential = Nothing
  },
  txOutValue = V3.Value $ fromValueNestedRep v,
  txOutDatum = V3.NoOutputDatum,
  txOutReferenceScript = Nothing
}

getOutChangeAddress :: TxOutChangeAddress -> V3.TxOut
getOutChangeAddress txout = do
  let pubkey = case txout of
        (TxOutChangeAddress (AddressShelley a)) -> case shelleyPayAddrToPlutusPubKHash a of
          Just b -> b
  V3.TxOut {
  txOutAddress = V3.Address {
    addressCredential = V3.PubKeyCredential pubkey,
    addressStakingCredential = Nothing
  },
  txOutValue = V3.Value AssocMap.empty,
  txOutDatum = V3.NoOutputDatum,
  txOutReferenceScript = Nothing
}

fromCardanoTxIn :: C.TxIn -> V3.TxOutRef
fromCardanoTxIn (C.TxIn txId (C.TxIx txIx)) = V3.TxOutRef (fromCardanoTxId txId) (toInteger txIx)

fromCardanoTxId :: C.TxId -> V3.TxId
fromCardanoTxId txId = V3.TxId $ fromString @BuiltinByteString $ show txId

getTxIns :: [(TxIn, Maybe (ScriptWitness WitCtxTxIn ConwayEra))] -> (V3.TxOutRef, V3.TxOutRef)
getTxIns [(tx1, Just (PlutusScriptWitness _ _ (PReferenceScript tx2 _) _ _ _))] = do
  let txOutRef_script = fromCardanoTxIn tx1
  let txOutRef_reference = fromCardanoTxIn tx2
  (txOutRef_script, txOutRef_reference)
getTxIns _ = undefined

initCmd :: IO ([V3.TxOutRef], [V3.TxOutRef], [V3.TxOut])
initCmd = toplevelExceptionHandler $ do
  Crypto.cryptoInit
  envCli <- getEnvCli
  GHC.mkTextEncoding "UTF-8" >>= GHC.setLocaleEncoding
  _ <- setFileCreationMask (otherModes `unionFileModes` groupModes)
  co <- Opt.customExecParser pref (opts envCli)
  case co of
    AnyEraCommand (AnyEraCommandOf era' (TransactionCmds trans)) ->
      case era' of
        ShelleyBasedEraConway -> do
          case trans of
            TransactionBuildCmd args -> do
              let insAndRefs = readScriptWitnessFiles (eon args) $ txins args
              -- ^ Transaction inputs with optional spending scripts
              -- let refInput :: [TxIn] = readOnlyReferenceInputs args
              -- ^ Read only reference inputs
              let outs = toTxOutInAnyEra (eon args) <$> txouts args
              -- ^ Normal outputs
              -- let lower = mValidityLowerBound args
              -- ^ Transaction validity lower bound
              -- let upper = mValidityUpperBound args
              -- ^ Transaction validity upper bound
              outs' <- mapM runExceptT outs
              insAndRefs' <- runExceptT insAndRefs
              let (_, outs'') = partitionEithers outs'
              let insAndRefs'' = case insAndRefs' of
                    Right x -> x
              let (tx1, tx2) = getTxIns insAndRefs''
              let outBob = getOutBob (head outs'')
              let outAdr = getOutChangeAddress $ changeAddresses args
              pure ([tx1], [tx2], [outAdr, outBob])

final :: ([V3.TxOutRef], [V3.TxOutRef], [V3.TxOut]) -> F
final ([tx1], [tx2], [outAdr, outBob]) =
  toZp @BLS12_381_Scalar $ convertToInteger .  convertToBuiltInByteString $ ([tx1], [tx2], [outAdr, outBob], always')
  where
    convertToBuiltInByteString :: ([V3.TxOutRef], [V3.TxOutRef], [V3.TxOut], POSIXTimeRange) -> BuiltinByteString
    convertToBuiltInByteString = blake2b_224 . serialiseData . toBuiltinData
    convertToInteger :: BuiltinByteString -> Integer
    convertToInteger = byteStringToInteger BigEndian . takeByteString 31
    always' :: POSIXTimeRange
    always' = always
-}

writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

main :: IO ()
main = do
  x           <- generate arbitrary
  ps          <- generate arbitrary
  targetValue <- generate arbitrary

  let contract = EqualityCheckContract x ps targetValue

  createDirectoryIfMissing True "../../test-data"
  createDirectoryIfMissing True "../../assets"

  BL.writeFile "test-data/symbolic-raw-contract-data.json" $ encode contract

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n" ++ "targetValue: " ++ show targetValue ++ "\n"

  let (setup, _, _) = equalityCheckVerificationBytes x ps targetValue

  savePlutus "../../assets/plonkVerifierTx.plutus" $ plonkVerifierTxCompiled setup
  savePlutus "../../assets/forwardingReward.plutus" forwardingRewardCompiled
