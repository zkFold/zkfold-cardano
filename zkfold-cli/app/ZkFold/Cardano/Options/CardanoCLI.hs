{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZkFold.Cardano.Options.CardanoCLI where

import           Cardano.CLI.EraBased.Options.Common (pSigningKeyFileOut)

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

