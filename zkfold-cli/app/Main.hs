module Main where

import           Cardano.Api                     (docToText)
import           Cardano.CLI.TopHandler          (toplevelExceptionHandler)
import qualified Cardano.Crypto.Init             as Crypto
import           Control.Monad.Trans.Except.Exit (orDie)
import qualified GHC.IO.Encoding                 as GHC
import qualified Options.Applicative             as Opt
import           Prelude                         (Bool (..), IO, Monad (..), ($), (.))
import           System.Directory                (createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath                 (takeFileName, (</>))

import           ZkFold.Cardano.Options.ZkCLI    (opts, pref, renderClientCommandError, runClientCommand)

main :: IO ()
main = toplevelExceptionHandler $ do
  Crypto.cryptoInit

  GHC.mkTextEncoding "UTF-8" >>= GHC.setLocaleEncoding

  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
          "e2e-test" -> ".."
          _          -> "."
  let testData = path </> "test-data"
      assets   = path </> "assets"

  createDirectoryIfMissing True testData
  createDirectoryIfMissing True assets

  co <- Opt.customExecParser pref (opts path)

  orDie (docToText . renderClientCommandError) $ runClientCommand co
