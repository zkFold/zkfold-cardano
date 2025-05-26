module ZkFold.Cardano.PlonkupVerifierTx.Transaction.Init where

import           Control.Exception                     (throwIO)
import           Data.Aeson                            (decode, encode)
import qualified Data.ByteString.Lazy                  as BL
import           Data.Maybe                            (fromJust, isJust)
import           GeniusYield.GYConfig                  (GYCoreConfig (cfgNetworkId), withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Prelude
import           System.Directory                      (createDirectoryIfMissing, doesFileExist)
import           System.FilePath                       ((</>))
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)

import           ZkFold.Cardano.Examples.IdentityCircuit (identityCircuitVerificationBytes)
import           ZkFold.Cardano.OnChain.Plonkup.Data   (SetupBytes)
import           ZkFold.Cardano.Options.Common
import           ZkFold.Cardano.PlonkupVerifierTx.Types
import           ZkFold.Cardano.UPLC.PlonkupVerifierTx (plonkupVerifierTxCompiled)


data Transaction = Transaction
  { curPath    :: !FilePath
  , coreCfgAlt :: !CoreConfigAlt
  }

displayPlonkupVerifierTxAddress :: GYNetworkId -> SetupBytes -> IO ()
displayPlonkupVerifierTxAddress nid setup = do
  let plutusValidator = plonkupVerifierTxCompiled setup
      script          = scriptFromPlutus @PlutusV3 plutusValidator
      scriptAddr      = addressFromValidator nid script

  putStr "\nplonkupVerifierTx Address:\n"
  print scriptAddr
  putStr "\n"

verifierInit :: Transaction -> IO ()
verifierInit (Transaction path coreCfg') = do
  let assetsPath = path </> "assets"
      setupFile  = assetsPath </> "plonkupVerifierTx-setup-data.json"

  createDirectoryIfMissing True assetsPath

  setupFileExists <- doesFileExist setupFile
  coreCfg         <- fromCoreConfigAltIO coreCfg'

  let nid = cfgNetworkId coreCfg

  if setupFileExists
    then do
      PlonkupVerifierTxSetup x <- fromJust . decode <$> BL.readFile setupFile
      ps <- generate arbitrary

      let (setup, _, _) = identityCircuitVerificationBytes x ps
      displayPlonkupVerifierTxAddress nid setup

    else do
      x           <- generate arbitrary
      ps          <- generate arbitrary

      let setupData = PlonkupVerifierTxSetup x
      BL.writeFile setupFile $ encode setupData

      let (setup, _, _) = identityCircuitVerificationBytes x ps
      displayPlonkupVerifierTxAddress nid setup
