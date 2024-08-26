#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./plonk/keys

echo "Fund alice, bob and charles"
echo "(Assuming someone has been funded from Faucet.)"

#----------------------------------- :funding: -----------------------------------

in1=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'to_entries | map(select(.value.value.lovelace > 500000000)) | .[0].key')

cardano-cli conway transaction build \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/someone.addr) + 500000000" \
  --tx-out "$(cat $keypath/alice.addr) + 500000000" \
  --tx-out "$(cat $keypath/bob.addr) + 500000000" \
  --tx-out "$(cat $keypath/charles.addr) + 500000000" \
  --change-address $(cat $keypath/someone.addr) \
  --out-file $keypath/tx.body \
  --testnet-magic 4

cardano-cli conway transaction sign \
  --tx-body-file $keypath/tx.body \
  --signing-key-file $keypath/someone.skey \
  --out-file $keypath/tx.signed \
  --testnet-magic 4

cardano-cli conway transaction submit \
  --tx-file $keypath/tx.signed \
  --testnet-magic 4

echo ""
echo "Pausing for 40 seconds..."
echo ""
sleep 40

echo "Someone's wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4)"
echo ""
echo "Alice's wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"
echo ""
echo "Bob's wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""
echo "Charles' wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/charles.addr) --testnet-magic 4)"
echo ""
