module Backend.Aux where

import           Prelude

-- Due to lack of composability of transactions built with cardano-cli, we need to
-- dynamically generate cardano-cli code for rollups referencing data updates.

cardanoCliCode :: Integer -> String
cardanoCliCode n = unlines $ firstCodeBlock ++ secondCodeBlock ++ thirdCodeBlock
  where
    firstCodeBlock =
      [ "#! /bin/bash"
      , "set -e"
      , "set -u"
      , "set -o pipefail"
      , "assets=../assets"
      , "keypath=./rollup/keys"
      , "privpath=./rollup/priv"
      , "mN=$(cat $privpath/testnet.flag)"
      , "state=$assets/datum.cbor"
      , "rollupRedeemer=$assets/redeemerRollup.cbor"
      , "rollupScript=$(cardano-cli transaction txid --tx-file \"$keypath/parkedScript.tx\")#0"
      , "nftPolicy=$assets/nftPolicy.plutus"
      , "nftPolicyId=$(cardano-cli conway transaction policyid --script-file $nftPolicy)"
      , "nftPolicyNm=\"7a6b466f6c64\""
      , "rollupLovelaceValue=3000000"
      , "rollupFee=15000000"
      , "in1=$(cardano-cli conway transaction txid --tx-file \"$keypath/rollupOut.tx\")#$aliceIdx"
      , "inR=$(cardano-cli transaction txid --tx-file \"$keypath/rollupOut.tx\")#0"
      , ""
      , "cardano-cli conway transaction build \\"
      , "  --testnet-magic $mN \\"
      , "  --tx-in $in1 \\"
      , "  --tx-in $inR \\"
      , "  --spending-tx-in-reference $rollupScript \\"
      , "  --spending-plutus-script-v3 \\"
      , "  --spending-reference-tx-in-inline-datum-present \\"
      , "  --spending-reference-tx-in-redeemer-cbor-file $rollupRedeemer \\"
      ]

    secondCodeBlock = map (\i -> "  --read-only-tx-in-reference $(cardano-cli conway transaction txid --tx-file \"$keypath/dataRef-" ++ show i ++ ".tx\")#0 \\") [0..n]

    thirdCodeBlock =
      [ "  --tx-in-collateral $in1 \\"
      , "  --tx-out \"$(cat $keypath/rollup.addr) + $rollupLovelaceValue lovelace + 1 $nftPolicyId.$nftPolicyNm\" \\"
      , "  --tx-out-inline-datum-cbor-file $state \\"
      , "  --tx-out \"$(cat $keypath/bob.addr) + $rollupFee lovelace\" \\"
      , "  --change-address $(cat $keypath/alice.addr) \\"
      , "  --out-file $keypath/rollupOut.txbody"
      ]

