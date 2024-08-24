#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets


cardano-cli conway address build \
    --payment-script-file "$assets/forwardingMint.plutus" \
    --out-file "$keypath/forwardingMint.addr" \
    --testnet-magic 4

