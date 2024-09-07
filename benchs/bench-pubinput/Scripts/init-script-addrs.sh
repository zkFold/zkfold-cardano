#! /bin/bash

set -e
set -u
set -o pipefail

keyspath="./Scripts/keys"
assetspath="../../assets"

cardano-cli conway address build \
    --payment-script-file "$assetspath/pubInput.plutus" \
    --out-file "$keyspath/pubInput.addr" \
    --testnet-magic 4


cardano-cli conway address build \
    --payment-script-file "$assetspath/alwaysSucceeds.plutus" \
    --out-file "$keyspath/alwaysSucceeds.addr" \
    --testnet-magic 4


