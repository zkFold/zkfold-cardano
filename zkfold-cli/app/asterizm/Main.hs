module Main where

import           Cardano.Api                        (docToText)
import           Cardano.CLI.TopHandler             (toplevelExceptionHandler)
import qualified Cardano.Crypto.Init                as Crypto
import           Control.Monad.Trans.Except.Exit    (orDie)
import qualified GHC.IO.Encoding                    as GHC
import qualified Options.Applicative                as Opt
import           Prelude

import           ZkFold.Cardano.Options.AsterizmCLI (opts, pref, renderClientCommandError, runClientCommand)

main :: IO ()
main = toplevelExceptionHandler $ do
  Crypto.cryptoInit

  GHC.mkTextEncoding "UTF-8" >>= GHC.setLocaleEncoding

  co <- Opt.customExecParser pref opts

  orDie (docToText . renderClientCommandError) $ runClientCommand co
