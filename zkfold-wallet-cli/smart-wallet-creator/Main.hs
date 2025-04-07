{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Cardano.Api
import           Cardano.Api.Shelley                         (PlutusScript (..))
import           Codec.Serialise                             as CBOR
import           Control.Monad                               (void)
import           Data.Aeson
import qualified Data.ByteString.Base64.URL                  as B64
import qualified Data.ByteString.Char8                       as C8
import qualified Data.ByteString.Lazy                        as BL
import           Data.Proxy
import           Data.String                                 (IsString (..))
import           Flat.Types                                  ()
import           GHC.Generics                                (Generic, Par1 (..), U1 (..), type (:*:) (..))
import qualified GHC.Generics                                as G
import           Options.Applicative
import           PlutusLedgerApi.Common                      as V3
import           PlutusLedgerApi.V3                          as V3
import           PlutusTx                                    (CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                            (BuiltinUnit)
import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           System.Directory                            (createDirectoryIfMissing, getCurrentDirectory)
import           System.Environment                          (getArgs)
import           System.FilePath                             (takeFileName, (</>))

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..), zero)
import qualified ZkFold.Base.Algebra.Basic.Number            as Number
import           ZkFold.Base.Algebra.Basic.Number            (Natural, type (^))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.Class     (CyclicGroup (..))
import           ZkFold.Base.Data.Vector                     (Vector)
import           ZkFold.Base.Protocol.NonInteractiveProof as NP   (NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonkup                (Plonkup (..))
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret (..))
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams, getSecrectParams)
import           ZkFold.Base.Protocol.Plonkup.Witness        (PlonkupWitnessInput (..))
import           ZkFold.Cardano.OffChain.Plonkup             (PlonkupN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.OnChain.BLS12_381.F          (toF)
import           ZkFold.Cardano.OnChain.Plonkup              (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data         (InputBytes, ProofBytes (..), SetupBytes)
import           ZkFold.Cardano.UPLC.Wallet                  (SpendingCreds (..), WalletRedeemer (..), WalletSetup (..),
                                                              Web2Creds (..), untypedWallet)
import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Cardano.Contracts.ZkLogin   (PublicInput, zkLogin)
import           ZkFold.Symbolic.Class                       (Symbolic (..))
import qualified ZkFold.Symbolic.Compiler                    as C
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..))
import           ZkFold.Symbolic.Data.Bool                   (Bool (..), true)
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement           (FieldElement)
import           ZkFold.Symbolic.Data.Input
import           ZkFold.Symbolic.Data.JWT                    as JWT
import           ZkFold.Symbolic.Data.JWT.Google
import           ZkFold.Symbolic.Data.JWT.RS256              as RS256
import           ZkFold.Symbolic.Data.UInt                   (UInt)
import           ZkFold.Symbolic.Data.VarByteString
import           ZkFold.Symbolic.Interpreter

import Debug.Trace (trace)
import qualified Prelude as P

data ZkLoginInput ctx =
    ZkLoginInput
        { zkHeader      :: TokenHeader ctx
        , zkPayload     :: GooglePayload ctx
        , zkSignature   :: JWT.Signature "RS256" ctx
        , zkAmount      :: ByteString 64 ctx
        , zkRecipient   :: ByteString 256 ctx
        , zkCertificate :: RS256.Certificate ctx
        , zkPI          :: PublicInput ctx
        }
        deriving Generic

deriving instance
    ( Symbolic ctx
    , KnownRegisters ctx PubExponentSize 'Auto
    , KnownRegisters ctx 2048 'Auto
    ) => SymbolicData (ZkLoginInput ctx)

instance
    ( Symbolic ctx
    , KnownRegisters ctx PubExponentSize 'Auto
    , KnownRegisters ctx 2048 'Auto
    ) => SymbolicInput (ZkLoginInput ctx) where
    isValid ZkLoginInput{..} = isValid zkAmount

zkLoginMock
    :: forall ctx
    .  RSA 2048 10328 ctx
    => TokenBits (GooglePayload ctx)
    => ZkLoginInput ctx
    -> Bool ctx
zkLoginMock _ = true

data ValidationData =
    ValidationData
        { vTokenHeader  :: String
        , vTokenPayload :: String
        , vSignature    :: String
        , vCertificate  :: String
        , vAmount       :: Natural
        , vRecipient    :: String
        , vPi           :: String
        , vUserId       :: String
        , vPubKeyHash   :: String
        , vOutputDir    :: FilePath
        }

data CreationData =
    CreationData
        { cUserId     :: String
        , cPubKeyHash :: String
        , cOutputDir  :: FilePath
        }

data RunMode =
      Create CreationData
    | Validate ValidationData

validationDataP :: Parser ValidationData
validationDataP = ValidationData
    <$> strOption (long "header" <> metavar "BASE64URL" <> help "JWT header")
    <*> strOption (long "payload" <> metavar "BASE64URL" <> help "JWT payload")
    <*> strOption (long "signature" <> metavar "BASE64URL" <> help "JWT signature")
    <*> strOption (long "certificate" <> metavar "STR" <> help "Public key for JWT signature verification")
    <*> option auto (long "amount" <> metavar "UINT" <> help "Tx amount in lovelace")
    <*> strOption (long "recipient" <> metavar "STR" <> help "Tx recipient")
    <*> strOption (long "input" <> metavar "BASE64URL" <> help "Wallet contract public input")
    <*> strOption (long "id" <> metavar "STR" <> help "User ID (email)")
    <*> strOption (long "pubkey" <> metavar "STR" <> help "Public key hash")
    <*> strOption (long "output" <> metavar "STR" <> help "Output directory")

creationDataP :: Parser CreationData
creationDataP = CreationData
    <$> strOption (long "id" <> metavar "STR" <> help "User ID (email)")
    <*> strOption (long "pubkey" <> metavar "STR" <> help "Public key hash")
    <*> strOption (long "output" <> metavar "STR" <> help "Output directory")


runModeP :: Parser RunMode
runModeP = (flag' () (long "create")   *> (Create <$> creationDataP))
       <|> (flag' () (long "validate") *> (Validate <$> validationDataP))

main :: IO ()
main = do
    let opts = info (runModeP <**> helper) (fullDesc <> progDesc "Produce a smart contract for the user")

    runMode <- execParser opts

    case runMode of
      Create (CreationData{..}) -> do
          createDirectoryIfMissing True cOutputDir
          savePlutus (cOutputDir </> "smartWallet.plutus") $ validator zkLoginSetupBytes (WalletSetup (fromString cUserId) (fromString cPubKeyHash))
      Validate valData -> do
          createDirectoryIfMissing True $ vOutputDir valData
          let (proofBytes, inputBytes) = zkLoginProofBytes valData
          let redeemer = zkLoginRedeemer valData proofBytes inputBytes
          let bytes = CBOR.serialise . toData . toBuiltinData $ redeemer
          BL.writeFile (vOutputDir valData </> "proof.cbor") bytes


uglyHardcodedPayloadInput = ((((U1 :*: U1) :*: ((U1 :*: U1) :*: (U1 :*: U1)))
                             :*: (((((U1 :*: U1) :*: ((U1 :*: U1) :*: (U1 :*: U1)))
                                    :*: (((U1 :*: U1) :*: (U1 :*: U1))
                                         :*: ((U1 :*: U1) :*: (U1 :*: U1))))
                                   :*: (((U1 :*: U1) :*: ((U1 :*: U1) :*: (U1 :*: U1)))
                                        :*: (((U1 :*: U1) :*: (U1 :*: U1))
                                             :*: ((U1 :*: U1) :*: (U1 :*: U1)))))
                                  :*: U1))
                            :*: ((U1 :*: U1) :*: (((U1 :*: U1) :*: (U1 :*: U1)) :*: U1)))
                           :*: U1


type NGates = 256 

zkLoginSetupBytes :: SetupBytes
zkLoginSetupBytes = mkSetup setupV
    where
        x = zero -- TODO: just to test compilation

        ac = C.compile @Fr zkLoginMock

        (omega, k1, k2) = getParams (Number.value @NGates)
        (gs, h1) = getSecrectParams @NGates @BLS12_381_G1_Point @BLS12_381_G2_Point x
        plonkup = Plonkup omega k1 k2 ac h1 gs
        setupV  = setupVerify @(PlonkupN _ _ NGates) plonkup

zkLoginRedeemer :: ValidationData -> ProofBytes -> InputBytes -> WalletRedeemer
zkLoginRedeemer ValidationData{..} proofBytes inputBytes =
    WalletRedeemer "1741153669" (fromString vRecipient) proofBytes inputBytes (SpendWithWeb2Token $ Web2Creds (fromString vUserId) "" (fromIntegral vAmount))

zkLoginProofBytes :: ValidationData -> (ProofBytes, InputBytes)
zkLoginProofBytes ValidationData{..} = (mkProof proof, mkInput pInput)
    where
        Just th  = decodeStrict . B64.decodeLenient . C8.pack $ vTokenHeader
        Just tp  = decodeStrict . B64.decodeLenient . C8.pack $ vTokenPayload
        ts       = fromConstant . B64.decodeLenient . C8.pack $ vSignature

        Just cert = decodeStrict . B64.decodeLenient . C8.pack $ vCertificate

        amount    = fromConstant vAmount
        recipient = fromString vRecipient
        pubi      = fromString vPi

        input :: ZkLoginInput (Interpreter Fr)
        input = ZkLoginInput th tp ts amount recipient cert pubi

        x = zero -- TODO: just to test compilation
        ps = PlonkupProverSecret $ pure zero

        ac = C.compile @Fr zkLoginMock

        witnessInputs   = runInterpreter $ arithmetize input Proxy

        (omega, k1, k2) = getParams (Number.value @NGates)
        (gs, h1) = getSecrectParams @NGates @BLS12_381_G1_Point @BLS12_381_G2_Point x
        plonkup = Plonkup omega k1 k2 ac h1 gs
        setupP  = setupProve @(PlonkupN _ _ NGates) plonkup
        setupV  = setupVerify @(PlonkupN _ _ NGates) plonkup
        witness = (PlonkupWitnessInput uglyHardcodedPayloadInput (witnessInputs :*: U1), ps)
        (pInput, proof) = prove @(PlonkupN _ _ NGates) setupP witness

validator :: SetupBytes -> WalletSetup -> CompiledCode (BuiltinData -> BuiltinUnit)
validator zkp ws =
    $$(PlutusTx.compile [|| untypedWallet ||])
    `unsafeApplyCode` liftCodeDef zkp
    `unsafeApplyCode` liftCodeDef ws


-- | Write serialized script to a file.
writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

-- | Serialize plutus script
savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . serialiseCompiledCode
