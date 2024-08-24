#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets

echo ""
echo "bob burning tokens."
echo ""

key=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'to_entries | map(select((.value.value | keys | length) == 1 and .value.value.lovelace > 10000000)) | .[0].key')

echo $key

