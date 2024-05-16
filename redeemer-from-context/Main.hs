{-# OPTIONS_GHC -Wno-name-shadowing      #-}
{-# OPTIONS_GHC -Wno-missing-signatures  #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import           Data.Map                                    (fromList)
import qualified Data.Map.Strict                             as Map
import           Data.String                                 (IsString (..))
import           GHC.ByteOrder                               (ByteOrder (..))
import qualified GHC.IO.Encoding                             as GHC
import qualified Options.Applicative                         as Opt
import           PlutusLedgerApi.V3                          (BuiltinByteString, CurrencySymbol (..), POSIXTimeRange, TokenName (..), adaSymbol, adaToken, always)
import qualified PlutusLedgerApi.V3                          as V3
import           PlutusTx                                    (ToData (..))
import qualified PlutusTx.AssocMap                           as AssocMap
import           PlutusTx.Builtins                           (blake2b_256, byteStringToInteger, serialiseData)
import           PlutusTx.Prelude                            (takeByteString)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)

import           ZkFold.Base.Algebra.Basic.Class             (AdditiveGroup (..), FromConstant (..))
import           ZkFold.Base.Algebra.Basic.Field             (toZp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar, Fr)
import           ZkFold.Base.Protocol.ARK.Plonk              (F, Plonk (..), PlonkWitnessInput (..))
import           ZkFold.Base.Protocol.ARK.Plonk.Internal     (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk.OffChain               (Contract (..), Plonk32, RowContractJSON, mkInput, mkProof, mkSetup, toContract)
import           ZkFold.Cardano.ScriptsVerifier              (RedeemerVerifier (..))
import           ZkFold.Symbolic.Cardano.Types               (TxId (..))
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit  (applyArgs)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Types                       (Symbolic)

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
    convertToBuiltInByteString = blake2b_256 . serialiseData . toBuiltinData
    convertToInteger :: BuiltinByteString -> Integer
    convertToInteger = byteStringToInteger BigEndian . takeByteString 31
    always' :: POSIXTimeRange
    always' = always

main :: IO ()
main = do
  (ins, refs, outs) <- initCmd -- TODO: add parse range
  jsonRowContract <- BL.readFile "../test-data/raw-contract-data.json"
  let maybeRowContract = decode jsonRowContract :: Maybe RowContractJSON
  case maybeRowContract of
    Just rowContract ->
      let Contract{..} = toContract rowContract
          lockedByTxId :: forall a a' . (Symbolic a , FromConstant a' a) => TxId a' -> TxId a -> Bool a
          lockedByTxId (TxId targetId) (TxId txId) = txId == fromConstant targetId

          Bool ac = compile @Fr (lockedByTxId @(ArithmeticCircuit Fr) @Fr (TxId targetId))
          acc = applyArgs ac [targetId]

          (omega, k1, k2) = getParams 5
          inputs  = fromList [(acOutput acc, 1)] -- final (a, b, c)
          plonk   = Plonk omega k1 k2 inputs acc x
          setup'  = setup @Plonk32 plonk
          w       = (PlonkWitnessInput inputs, ps)
          (input', proof') = prove @Plonk32 setup' w
      in do
        let setup = mkSetup setup'
            input = mkInput input'
            proof = mkProof setup proof'
            redeemer = RedeemerVerifier setup input proof
        -- print $ "input" ++ show input
        print $ "final input:" ++ show (negate $ final (ins, refs, outs))
        BS.writeFile ".././assets/redeemer.json" (prettyPrintJSON $ dataToJSON redeemer)
        BS.writeFile ".././assets/redeemer.cbor" (B16.encode (serialize' $ encode (V3.Redeemer . toBuiltinData $ redeemer)))
    _ -> print ("Could not deserialize" :: String)

