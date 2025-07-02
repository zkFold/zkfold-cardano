module ZkFold.Cardano.Options.Common where

import           Cardano.Api                        (parseAddressAny)
import           Cardano.CLI.EraBased.Common.Option (readerFromParsecParser)
import           GeniusYield.Types                  as GY
import           Options.Applicative                (Parser)
import qualified Options.Applicative                as Opt
import           Prelude

--------------------------- :Defaults: ---------------------------

-- | Name of data tokens mint Tx out-file.
dataOut :: String
dataOut = "dataTokens.tx"

------------------------ :simple parsers: ------------------------

pFeeAddress :: Parser GYAddress
pFeeAddress =
    Opt.option (readerFromParsecParser $ fmap GY.addressFromApi parseAddressAny) $
        mconcat
            [ Opt.long "fee-address"
            , Opt.metavar "ADDRESS"
            , Opt.help "Tx out address."
            ]

----------------------- :parsing OutFile: ------------------------

data StageTx = RollupInit | RollupPark | RollupDataPark | RollupUpdate | RollupClear

class HasFileParser a where
  outFileName :: a -> String
  outFileFlag :: a -> String
  outFileHelp :: a -> String

  pOutFile :: a -> Parser FilePath
  pOutFile x = Opt.strOption
    ( Opt.long (outFileFlag x)
        <> Opt.value (outFileName x)
        <> Opt.metavar "FILEPATH"
        <> Opt.help (outFileHelp x)
        <> Opt.completer (Opt.bashCompleter "file")
    )

instance HasFileParser StageTx where
  outFileFlag RollupInit     = "init-out-file"
  outFileFlag RollupPark     = "park-out-file"
  outFileFlag RollupDataPark = "data-park-out-file"
  outFileFlag RollupUpdate   = "update-out-file"
  outFileFlag RollupClear    = "clear-out-file"

  outFileName RollupInit     = "rollup-init.tx"
  outFileName RollupPark     = "rollup-park.tx"
  outFileName RollupDataPark = "rollup-data-park.tx"
  outFileName RollupUpdate   = "rollup-update.tx"
  outFileName RollupClear    = "rollup-clear.tx"

  outFileHelp RollupInit     = "Path (relative to 'assets/') for rollup initialization tx out-file."
  outFileHelp RollupPark     = "Path (relative to 'assets/') for 'rollup' script-parking tx out-file."
  outFileHelp RollupDataPark = "Path (relative to 'assets/') for 'rollupData' script-parking tx out-file."
  outFileHelp RollupUpdate   = "Path (relative to 'assets/') for rollup update tx out-file."
  outFileHelp RollupClear    = "Path (relative to 'assets/') for rollup token-clearing tx out-file."
