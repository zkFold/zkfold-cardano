#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets

pause=7

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo "Rollup Alice funds..."

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/alice.addr) + 97000000000 lovelace" \
  --change-address $(cat $keypath/alice.addr) \
  --out-file $keypath/splitAlice.txbody

cardano-cli conway transaction sign \
  --testnet-magic 4 \
  --tx-body-file $keypath/splitAlice.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/splitAlice.tx

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file $keypath/splitAlice.tx
