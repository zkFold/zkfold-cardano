#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath0=../local-testnet/example/utxo-keys
keypath=./keys
mN=42

echo "Fund Alice..."

#----------------------------------- :utxo1: -----------------------------------

cardano-cli conway address build \
  --payment-verification-key-file $keypath0/utxo1.vkey \
  --out-file $keypath0/utxo1.addr \
  --testnet-magic $mN

#----------------------------------- :funding: -----------------------------------

# echo "$(cardano-cli query utxo --address $(cat $keypath0/utxo1.addr) --testnet-magic $mN)"

in1=$(cardano-cli query utxo --address $(cat $keypath0/utxo1.addr) --testnet-magic $mN --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/alice.addr) + 1000000000" \
  --change-address $(cat $keypath0/utxo1.addr) \
  --out-file $keypath/tx.body \
  --testnet-magic $mN

cardano-cli conway transaction sign \
  --tx-body-file $keypath/tx.body \
  --signing-key-file $keypath0/utxo1.skey \
  --out-file $keypath/tx.signed \
  --testnet-magic $mN

cardano-cli conway transaction submit \
  --tx-file $keypath/tx.signed \
  --testnet-magic $mN

echo ""
echo "Pausing for 5 seconds..."
echo ""
sleep 5

echo "Alice's wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN)"
echo ""
