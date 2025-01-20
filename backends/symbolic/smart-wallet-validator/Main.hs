{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Cardano.Api
import           Cardano.Api.Shelley                 (PlutusScript (..))
import           Control.Monad                       (void)
import           Data.String                         (IsString (..))
import           Flat.Types                          ()
import           PlutusLedgerApi.V3                  as V3
import           PlutusTx                            (CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import           PlutusTx.Prelude                    (BuiltinUnit)
import           Prelude                             hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import           System.Directory                    (createDirectoryIfMissing, getCurrentDirectory)
import           System.Environment                  (getArgs)
import           System.FilePath                     (takeFileName, (</>))

import           ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import           ZkFold.Cardano.UPLC.Wallet          (WalletSetup (..), untypedWallet)

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "rollup"   -> ".." </> ".."
        "e2e-test" -> ".."
        _          -> "."

  createDirectoryIfMissing True $ path </> "test-data"
  createDirectoryIfMissing True $ path </> "assets"
  argsRaw <- getArgs

  case argsRaw of
    (rawZkp : rawUserId : rawPubKeyHash : _) -> do
        -- TODO Symbolic function to SetupBytes?
        let zkp = undefined rawZkp
            userId = fromString rawUserId
            pubKeyHash = fromString rawPubKeyHash
        savePlutus (path </> "assets" </> "walletScript.plutus") $ validator zkp (WalletSetup userId pubKeyHash)

    _ -> error "Error: please provide three command-line arguments.\n"



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
