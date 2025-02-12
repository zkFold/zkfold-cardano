module ZkFold.Cardano.Rollup.CardanoCli where

import           Prelude

-- Due to lack of composability of transactions built with cardano-cli, we need to dynamically
-- generate cardano-cli code for transactions with variable inputs and/or variable minting
-- parameters.

initialCodeBlock :: [String]
initialCodeBlock =
  [ "#! /bin/bash"
  , "set -e"
  , "set -u"
  , "set -o pipefail"
  , "assets=../assets"
  , "keypath=./rollup/keys"
  , "privpath=./rollup/priv"
  , "mN=$(cat $privpath/testnet.flag)"
  , "unitDatum=$assets/unit.cbor"
  , "unitRedeemer=$assets/unit.cbor"
  , "dataPolicy=$assets/rollupData.plutus"
  , "dataCleanRedeemer=$assets/dataCleanRedeemer.cbor"
  , "parkingSpotPolicy=$assets/parkingSpot.plutus"
  , "collateral=$(cardano-cli conway transaction txid --tx-file \"$keypath/splitAlice.tx\")#0"
  , ""
  , "in1=$(cardano-cli conway transaction txid --tx-file \"$keypath/rollupOut.tx\")#3"
  , "dataPolicyId=$(cardano-cli conway transaction policyid --script-file $dataPolicy)"
  , ""
  , "cardano-cli conway transaction build \\"
  , "  --testnet-magic $mN \\"
  , "  --tx-in $in1 \\"
  ]

middleCodeBlock :: [String]
middleCodeBlock =
  [ "  --tx-in-collateral $collateral \\"
  , "  --change-address $(cat $keypath/alice.addr) \\"
  ]

finalCodeBlock :: [String]
finalCodeBlock =
  [ "  --mint-script-file $dataPolicy \\"
  , "  --mint-redeemer-cbor-file $dataCleanRedeemer \\"
  , "  --out-file $keypath/dataClean.txbody"
  ]

inputCodeBlock :: String -> [String]
inputCodeBlock s =
  [ "  --tx-in " ++ s ++ " \\"
  , "  --tx-in-script-file $parkingSpotPolicy \\"
  , "  --tx-in-inline-datum-present \\"
  , "  --tx-in-redeemer-cbor-file $unitRedeemer \\"
  ]

mintCodeBlock :: [String] -> [String]
mintCodeBlock tokenNames = ["  --mint \"" ++ concat (zipWith zipper tokenNames wrapUps)]
  where
    zipper tn s = "-1 $dataPolicyId"  ++ "." ++ tn ++ s
    wrapUps     = replicate (length tokenNames - 1) " + " ++ ["\" \\"]
