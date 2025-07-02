module Main where

import           Cardano.Api                        (docToText)
import           Cardano.CLI.TopHandler             (toplevelExceptionHandler)
import qualified Cardano.Crypto.Init                as Crypto
import           Control.Monad.Trans.Except.Exit    (orDie)
import           GeniusYield.GYConfig               (coreConfigIO)
import qualified GHC.IO.Encoding                    as GHC
import qualified Options.Applicative                as Opt
import           Prelude
import           System.Directory                   (createDirectoryIfMissing, getCurrentDirectory)
import           System.Environment                 (lookupEnv)
import           System.FilePath                    (takeFileName, (</>))

import           ZkFold.Cardano.Options.AsterizmCLI (opts, pref, renderClientCommandError, runClientCommand)

main :: IO ()
main = toplevelExceptionHandler $ do
  Crypto.cryptoInit

  GHC.mkTextEncoding "UTF-8" >>= GHC.setLocaleEncoding

  coreCfgEnv <- lookupEnv "CORE_CONFIG_PATH"
  mCoreCfg   <- maybe (pure Nothing) (\path -> Just <$> coreConfigIO path) coreCfgEnv

  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "asterizm" -> "../.."
        "e2e-test" -> ".."
        _          -> "."
  let assets = path </> "assets"

  createDirectoryIfMissing True assets

  co <- Opt.customExecParser pref (opts path mCoreCfg)

  orDie (docToText . renderClientCommandError) $ runClientCommand co
