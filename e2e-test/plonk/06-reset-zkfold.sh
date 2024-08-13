#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys

echo "Reset zkfold-main."

#-------------------------------- :zkfold-main: --------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/zkfold-main.vkey \
  --signing-key-file $keypath/zkfold-main.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/zkfold-main.vkey \
  --out-file $keypath/zkfold-main.addr \
  --testnet-magic 4

