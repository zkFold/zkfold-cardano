{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZkFold.Cardano.Options.CardanoCLI where

import           Cardano.Api
import           Cardano.CLI.EraBased.Options.Common
import           Options.Applicative
import qualified Options.Applicative                 as Opt
import           Prelude

pChangeAddress :: Parser AddressAny
pChangeAddress =
    Opt.option (readerFromParsecParser parseAddressAny) $
      mconcat
        [ Opt.long "change-address"
        , Opt.metavar "ADDRESS"
        , Opt.help "Address where ADA in excess of the tx fee will go to."
        ]

pGYCoreConfig :: Parser FilePath
pGYCoreConfig = parseFilePath "path-to-gycoreconfig" "Path to GYCoreConfig."

pOutAddress :: Parser AddressAny
pOutAddress =
  Opt.option (readerFromParsecParser parseAddressAny) $
    mconcat
      [ Opt.long "out-address"
      , Opt.metavar "ADDRESS"
      , Opt.help "Tx out address."
      ]

pTxInOnly :: Parser TxIn
pTxInOnly =
  Opt.option
    (readerFromParsecParser parseTxIn)
    ( Opt.long "tx-in"
        <> Opt.metavar "TX-IN"
        <> Opt.help "TxId#TxIx"
    )

pOutFile :: Parser FilePath
pOutFile = parseFilePath "tx-out" "Tx out address."

-- paymentSigningKeyFromApi + pSigningKeyFileOut

-- pChangeAddress :: Parser TxOutChangeAddress
-- newtype TxOutChangeAddress = TxOutChangeAddress AddressAny
-- addressFromApi

-- Api.AddressAny -> GYAddress

-- pTxInOnly :: Parser TxIn
{-
txOutRefFromApi :: Api.TxIn -> GYTxOutRef
-}
{-
data GYTxIn v = GYTxIn
  { gyTxInTxOutRef :: !GYTxOutRef
  , gyTxInWitness :: !(GYTxInWitness v)
  }
  deriving (Eq, Show)

-- | Represents witness type and associated information for tx inputs.
data GYTxInWitness v
  = -- | Key witness without datum.
    GYTxInWitnessKey
  | -- | Script witness with associated script, datum, and redeemer. Datum can be omitted if it is inlined in the input or if it's not needed under PlutusV3 (or beyond) script.
    GYTxInWitnessScript !(GYBuildPlutusScript v) !(Maybe GYDatum) !GYRedeemer
  | -- | Simple script witness.
    GYTxInWitnessSimpleScript !(GYBuildSimpleScript v)
  deriving stock (Eq, Show)

-}

-- fix  tx-out
-- parseAddressAny :: Parsec.Parser AddressAny
{-
-- This parser renders the appropriate parsers depending on what
-- functionality is available per era.
pTxOutEraAware :: ShelleyBasedEra era -> Parser TxOutAnyEra
pTxOutEraAware sbe =
  Opt.option
    (readerFromParsecParser parseTxOutAnyEra)
    ( Opt.long "tx-out"
        <> Opt.metavar "ADDRESS VALUE"
        -- TODO alonzo: Update the help text to describe the new syntax as well.
        <> Opt.help
          "The transaction output as ADDRESS VALUE where ADDRESS is \
          \the Bech32-encoded address followed by the value in \
          \the multi-asset syntax (including simply Lovelace)."
    )
    <*> pTxOutDatum sbe
    <*> pRefScriptFp sbe
-}

-- pReferenceTxIn :: String -> String -> Parser TxIn

