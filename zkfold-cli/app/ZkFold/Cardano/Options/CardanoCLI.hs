module ZkFold.Cardano.Options.CardanoCLI where

import           Cardano.Api                         (AddressAny, TxIn, parseAddressAny)
import           Cardano.CLI.EraBased.Options.Common (parseFilePath, parseTxIn, readerFromParsecParser)
import           Options.Applicative                 (Parser)
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

pTxIdFile :: Parser FilePath
pTxIdFile = parseFilePath "tx-id" "Tx id address."
