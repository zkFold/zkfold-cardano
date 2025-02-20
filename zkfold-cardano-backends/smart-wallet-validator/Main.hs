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

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Protocol.NonInteractiveProof    (HaskellCore, NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonkup                (Plonkup (..))
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams)
import           ZkFold.Base.Protocol.Plonkup.Witness        (PlonkupWitnessInput (..))
import           ZkFold.Cardano.OffChain.Plonkup             (PlonkupN, mkInput, mkProof, mkSetup)
import           ZkFold.Cardano.OnChain.Plonkup              (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data         (InputBytes, ProofBytes, SetupBytes)
import           ZkFold.Cardano.UPLC.Wallet                  (WalletSetup (..), untypedWallet)
import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Cardano.Contracts.ZkLogin   (zkLogin)
import           ZkFold.Symbolic.Class                       (Symbolic (..))
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compile)
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
        , amount       :: Natural
        , recipient    :: String
        , certificate  :: String
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

    pure ()

zkLoginSetupBytes :: SetupBytes
zkLoginSetupBytes = mkSetup setupV
    where
        ac = compile @Fr (zkLogin @Fr @(ArithmeticCircuit Fr (U1 :*: U1) (Par1 :*: U1))) :: ArithmeticCircuit Fr (U1 :*: U1) (Par1 :*: U1) Par1

        (omega, k1, k2) = getParams (2 ^ 24)
        plonkup = Plonkup omega k1 k2 ac x :: PlonkupN (U1 :*: U1) (Par1 :*: U1) 32
        setupV  = setupVerify @_ @HaskellCore plonkup



validator :: SetupBytes -> WalletSetup -> CompiledCode (BuiltinData -> BuiltinUnit)
validator zkp ws =
    $$(compile [|| untypedWallet ||])
    `unsafeApplyCode` liftCodeDef zkp
    `unsafeApplyCode` liftCodeDef ws


-- | Write serialized script to a file.
writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = void $ writeFileTextEnvelope (File filePath) Nothing script

-- | Serialize plutus script
savePlutus :: FilePath -> CompiledCode a -> IO ()
savePlutus filePath =
  writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . serialiseCompiledCode
