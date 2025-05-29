{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Cardano.PlonkupVerifierTx.Types where

import           Data.Aeson
import           GHC.Generics
import           Prelude

import           ZkFold.Algebra.EllipticCurve.BLS12_381 (Fr)


data PlonkupVerifierTxSetup = PlonkupVerifierTxSetup { pvtX :: Fr }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
