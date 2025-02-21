{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Cardano.Api
import           Cardano.Api.Shelley                         (PlutusScript (..))
import           Control.Monad                               (void)
import           Data.String                                 (IsString (..))
import           Flat.Types                                  ()
import           GHC.Generics                                (Generic, Par1 (..), U1 (..), type (:*:) (..))
import           Options.Applicative
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
import           ZkFold.Base.Protocol.NonInteractiveProof    (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonkup                (Plonkup (..))
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams, getSecrectParams)
import           ZkFold.Base.Protocol.Plonkup.Witness        (PlonkupWitnessInput (..))
import           ZkFold.Cardano.OffChain.Plonkup             (PlonkupN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.OnChain.Plonkup              (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data         (InputBytes, ProofBytes, SetupBytes)
import           ZkFold.Cardano.UPLC.Wallet                  (WalletSetup (..), untypedWallet)
import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Cardano.Contracts.ZkLogin   (zkLogin, zkLoginMock)
import           ZkFold.Symbolic.Class                       (Symbolic (..))
import qualified ZkFold.Symbolic.Compiler                    as C
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..))
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement           (FieldElement)
import           ZkFold.Symbolic.Data.JWT                    (Certificate, ClientSecret (..), SecretBits,
                                                              TokenPayload (..), verifySignature)
import           ZkFold.Symbolic.Data.VarByteString

data UserData =
    UserData
        { tokenHeader  :: String
        , tokenPayload :: String
        , signature    :: String
        , certificate  :: String
        , amount       :: Natural
        , recipient    :: String
        , pi           :: String
        , userId       :: String
        , pubKeyHash   :: String
        }

userData :: Parser UserData
userData = UserData
    <$> strOption (long "header" <> metavar "BASE64URL" <> help "JWT header")
    <*> strOption (long "payload" <> metavar "BASE64URL" <> help "JWT payload")
    <*> strOption (long "signature" <> metavar "BASE64URL" <> help "JWT signature")
    <*> strOption (long "certificate" <> metavar "STR" <> help "Public key for JWT signature verification")
    <*> option auto (long "amount" <> metavar "UINT" <> help "Tx amount in lovelace")
    <*> strOption (long "recipient" <> metavar "STR" <> help "Tx recipient")
    <*> strOption (long "input" <> metavar "BASE64URL" <> help "Wallet contract public input")
    <*> strOption (long "id" <> metavar "STR" <> help "User ID (email)")
    <*> strOption (long "pubkey" <> metavar "STR" <> help "Public key hash")


main :: IO ()
main = do
    currentDir <- getCurrentDirectory
    let path = case takeFileName currentDir of
          "rollup"   -> ".." </> ".."
          "e2e-test" -> ".."
          _          -> "."

    createDirectoryIfMissing True $ path </> "test-data"
    createDirectoryIfMissing True $ path </> "assets"

    let opts = info (userData <**> helper) (fullDesc <> progDesc "Produce a smart contract for the user")

    UserData{..} <- execParser opts

    savePlutus (path </> "assets" </> "smartWallet.plutus") $ validator zkLoginSetupBytes (WalletSetup (fromString userId) (fromString pubKeyHash))

type NGates = 2^16

zkLoginSetupBytes :: SetupBytes
zkLoginSetupBytes = mkSetup setupV
    where
        x = zero -- TODO: just to test compilation

        ac = C.compile @Fr zkLoginMock

        (omega, k1, k2) = getParams (Number.value @NGates)
        (gs, h1) = getSecrectParams @NGates @BLS12_381_G1_Point @BLS12_381_G2_Point x
        plonkup = Plonkup omega k1 k2 ac h1 gs -- :: PlonkupN Inp Out NGates
        setupV  = setupVerify @(PlonkupN _ _ NGates) @HaskellCore plonkup



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
