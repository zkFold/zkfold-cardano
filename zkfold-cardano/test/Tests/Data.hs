{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}

module Tests.Data (verifyIsSatisfied) where


import           Data.Aeson                               (FromJSON, ToJSON, decode)
import           Data.ByteString                          (pack)
import qualified Data.ByteString.Lazy                     as BL
import           Data.Word                                ()

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.Inputs

verifyIsSatisfied :: IO ()
verifyIsSatisfied = do
    jsonDataProof <- BL.readFile "test-data/proof.json"
    jsonDataSetup <- BL.readFile "test-data/setup.json"
    jsonDataInput <- BL.readFile "test-data/input.json"
    let maybeProof = decode jsonDataProof :: Maybe ProofJSON
    let maybeSetup = decode jsonDataSetup :: Maybe SetupJSON
    let maybeInput = decode jsonDataInput :: Maybe InputJSON
    case (maybeProof, maybeSetup, maybeInput) of
      (Just prf, Just stp, Just inp) -> do
        let p = convertProofPlonkPlutus prf
        let s = convertSetupPlonkPlutus stp
        let i = convertInputPlonkPlutus inp
        print $ verify @PlonkPlutus s i p
      _ -> print "Could not deserialize"
