{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Cardano.PlonkupVerifierTx.Types where

import           Data.Aeson
import           Prelude
import           GHC.Generics

import           ZkFold.Algebra.EllipticCurve.BLS12_381 (Fr)


data PlonkupVerifierTxSetup = PlonkupVerifierTxSetup { pvtX :: Fr }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
