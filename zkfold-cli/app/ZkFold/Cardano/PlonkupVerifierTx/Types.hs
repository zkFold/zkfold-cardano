{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Cardano.PlonkupVerifierTx.Types where

import           Data.Aeson
import           GeniusYield.Types (GYAddress, GYValue)
import           Prelude
import           GHC.Generics

import           ZkFold.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Cardano.Options.Common


data PlonkupVerifierTxSetup = PlonkupVerifierTxSetup { pvtX :: !Fr }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Transfer = Transfer
  { reward         :: !GYValue
  , requiredSigner :: !SigningKeyAlt
  , changeAddress  :: !GYAddress
  , outFile        :: !FilePath
  } deriving stock Show
