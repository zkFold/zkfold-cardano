{-# LANGUAGE InstanceSigs #-}

module ZkFold.Cardano.Options.TmpExperiment where

import           Cardano.Api                        (AddressAny, TxIn, parseAddressAny, runExceptT)
import           Cardano.CLI.EraBased.Common.Option (parseFilePath, parseTxIn, readerFromParsecParser)

import           Cardano.CLI.EraBased.Common.Option (pScriptDataOrFile)
import           Cardano.CLI.Type.Common            (ScriptDataOrFile, TxOutAnyEra (..), TxOutDatumAnyEra (..))
import           Cardano.CLI.EraBased.Script.Read.Common (readScriptDataOrFile)

-- import           Control.Exception                  (throwIO)
import           Data.Aeson                         (decodeFileStrict)
import qualified Data.ByteString.Char8              as BS
import           Data.Maybe                         (fromJust)
import           Data.String                        (fromString)
-- import           GeniusYield.GYConfig               as GY
import           GeniusYield.Types                  as GY
import           Options.Applicative                (Parser)
import qualified Options.Applicative                as Opt
import           Prelude
-- import           System.FilePath                    ((</>))
-- import qualified System.IO                          as IO


whatsup :: [TxOutAnyEra] -> IO ()
whatsup (out1 : out2 : _) = do
  let TxOutAnyEra _ _ dae1 _ = out1
  case dae1 of
    TxOutInlineDatumByValue sd -> do
      result <- runExceptT (readScriptDataOrFile sd)
      case result of
        Left err  -> putStrLn $ "Error reading script data: " ++ show err
        Right dat -> putStrLn $ "Script data read:\n" ++ show (GY.datumFromApi' dat)

    _ -> putStrLn "something else"

  let TxOutAnyEra addr _ _ _ = out2
  print $ GY.addressFromApi addr

seeTxIns :: [GYTxOutRef] -> IO ()
seeTxIns [] = pure ()
seeTxIns (oref : orefs) = print (GYTxIn oref GYTxInWitnessKey) >> seeTxIns orefs
  
