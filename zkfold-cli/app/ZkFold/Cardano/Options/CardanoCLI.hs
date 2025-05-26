{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZkFold.Cardano.Options.CardanoCLI where

import Cardano.Api

-- import Cardano.CLI.Compatible.Transaction.Command
-- import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Common.Option hiding (pRefScriptFp, pTxOutDatum, pVoteFiles)
-- import Cardano.CLI.EraBased.Script.Vote.Type
-- import Cardano.CLI.Parser
import Cardano.CLI.Type.Common
-- import Cardano.CLI.Type.Governance

import Data.Foldable hiding (toList)
import Options.Applicative
import Options.Applicative qualified as Opt
import Prelude


pTxOutEraAware :: ShelleyBasedEra era -> Parser TxOutAnyEra
pTxOutEraAware sbe =
  Opt.option
    (readerFromParsecParser parseTxOutAnyEra)
    ( Opt.long "tx-out"
        <> Opt.metavar "ADDRESS VALUE"
        <> Opt.help
          "The transaction output as ADDRESS VALUE where ADDRESS is \
          \the Bech32-encoded address followed by the value in \
          \the multi-asset syntax (including simply Lovelace)."
    )
    <*> pTxOutDatum sbe
    <*> pRefScriptFp sbe

pTxOutDatum :: ShelleyBasedEra era -> Parser TxOutDatumAnyEra
pTxOutDatum =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const $ pure TxOutDatumByNone)
    ( \case
        AlonzoEraOnwardsAlonzo ->
          pAlonzoDatumFunctionality <|> pure TxOutDatumByNone
        AlonzoEraOnwardsBabbage ->
          pBabbageDatumFunctionality <|> pure TxOutDatumByNone
        AlonzoEraOnwardsConway -> pConwayDatumFunctionality <|> pure TxOutDatumByNone
    )
 where
  pAlonzoDatumFunctionality =
    asum
      [ pTxOutDatumByHashOf
      , pTxOutDatumByValue
      ]
  pBabbageDatumFunctionality =
    asum
      [ pAlonzoDatumFunctionality
      , pTxOutInlineDatumByValue
      ]

  pConwayDatumFunctionality = pBabbageDatumFunctionality

  pTxOutDatumByHashOf =
    TxOutDatumByHashOf
      <$> pScriptDataOrFile
        "tx-out-datum-hash"
        "The script datum hash for this tx output, by hashing the script datum given here."
        "The script datum hash for this tx output, by hashing the script datum in the file."

  pTxOutDatumByValue =
    TxOutDatumByValue
      <$> pScriptDataOrFile
        "tx-out-datum-embed"
        "The script datum to embed in the tx for this output, given here."
        "The script datum to embed in the tx for this output, in the given file."

  pTxOutInlineDatumByValue =
    TxOutInlineDatumByValue
      <$> pScriptDataOrFile
        "tx-out-inline-datum"
        "The script datum to embed in the tx output as an inline datum, given here."
        "The script datum to embed in the tx output as an inline datum, in the given file."

pRefScriptFp :: ShelleyBasedEra era -> Parser ReferenceScriptAnyEra
pRefScriptFp =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ pure ReferenceScriptAnyEraNone)
    ( const $
        ReferenceScriptAnyEra
          <$> parseFilePath "tx-out-reference-script-file" "Reference script input file."
            <|> pure ReferenceScriptAnyEraNone
    )
