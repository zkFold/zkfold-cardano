#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys

echo "Set Alice collateral."

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/alice.addr) + 10000000" \
  --change-address $(cat $keypath/alice.addr) \
  --out-file $keypath/tx.body \
  --testnet-magic 4

cardano-cli conway transaction sign \
  --tx-body-file $keypath/tx.body \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/tx.signed \
  --testnet-magic 4

cardano-cli conway transaction submit \
  --tx-file $keypath/tx.signed \
  --testnet-magic 4

echo ""
echo "Pausing for 30 seconds..."
echo ""
sleep 30

echo "Alice:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"
