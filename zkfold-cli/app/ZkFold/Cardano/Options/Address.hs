{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZkFold.Cardano.Options.Address where

{-
import           Cardano.CLI.Commands.Address
import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Parser

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

pAddressCmds :: Parser AddressCmds
pAddressCmds envCli =
  let addressParsers =
        asum
          [ subParser "key-gen" $
              Opt.info pAddressKeyGen $
                Opt.progDesc "Create an address key pair."
          , subParser "key-hash" $
              Opt.info pAddressKeyHash $
                Opt.progDesc "Print the hash of an address key."
          , subParser "build" $
              Opt.info (pAddressBuild envCli) $
                Opt.progDesc "Build a Shelley payment address, with optional delegation to a stake address."
          , subParser "info" $
              Opt.info pAddressInfo $
                Opt.progDesc "Print information about an address."
          ]
   in subParser
        "address"
        $ Opt.info
          addressParsers
          ( Opt.progDesc $
              mconcat
                [ "Payment address commands."
                ]
          )

pAddressKeyGen :: Parser AddressCmds
pAddressKeyGen =
  AddressKeyGen
    <$> pKeyOutputFormat
    <*> pAddressKeyType
    <*> pVerificationKeyFileOut
    <*> pSigningKeyFileOut

pAddressKeyHash :: Parser AddressCmds
pAddressKeyHash =
  AddressKeyHash
    <$> pPaymentVerificationKeyTextOrFile
    <*> pMaybeOutputFile

pAddressBuild :: Parser AddressCmds
pAddressBuild =
  AddressBuild
    <$> pPaymentVerifier
    <*> Opt.optional (pStakeIdentifier Nothing)
    <*> pNetworkId
    <*> pMaybeOutputFile

pAddressInfo :: Parser AddressCmds
pAddressInfo =
  AddressInfo
    <$> pAddress
    <*> pMaybeOutputFile
-}