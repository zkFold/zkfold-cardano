#! /bin/bash

set -e
set -u
set -o pipefail

keypath="./scripts/keys"
assetspath="../../assets"

cardano-cli conway address build \
    --payment-script-file "$assetspath/alwaysSucceeds.plutus" \
    --out-file "$keypath/alwaysSucceeds.addr" \
    --testnet-magic 4

cardano-cli conway address build \
    --payment-script-file "$assetspath/symbolicVerifier.plutus" \
    --out-file "$keypath/symbolicVerifier.addr" \
    --testnet-magic 4
