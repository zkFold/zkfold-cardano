#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

sanchomagic=4
keypath=./plonkupVerifierToken/keys
privpath=./plonkupVerifierToken/priv

magic=$(cat $privpath/testnet.flag)
if [ $magic -ne $sanchomagic ]; then
    echo "This script is only meant to be run on SanchoNet."
    exit 1
fi

#----------------------------------- :funding: -----------------------------------

echo "Fund alice, bob and charles"
echo "(Assuming someone has been funded from Faucet.)"
echo ""
echo "someone address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic $magic)"
echo ""

in1=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic $magic --out-file  /dev/stdout | jq -r 'to_entries | map(select(.value.value.lovelace > 500000000)) | .[0].key')

cardano-cli conway transaction build \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/someone.addr) + 500000000" \
  --tx-out "$(cat $keypath/alice.addr) + 500000000" \
  --tx-out "$(cat $keypath/bob.addr) + 500000000" \
  --tx-out "$(cat $keypath/charles.addr) + 500000000" \
  --change-address $(cat $keypath/someone.addr) \
  --out-file $keypath/tx.body \
  --testnet-magic $magic

cardano-cli conway transaction sign \
  --tx-body-file $keypath/tx.body \
  --signing-key-file $keypath/someone.skey \
  --out-file $keypath/tx.signed \
  --testnet-magic $magic

cardano-cli conway transaction submit \
  --tx-file $keypath/tx.signed \
  --testnet-magic $magic

echo ""
echo "Pausing for 50 seconds..."
echo ""
sleep 50

echo "Someone's wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/someone.addr) --testnet-magic $magic)"
echo ""
echo "Alice's wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $magic)"
echo ""
echo "Bob's wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic)"
echo ""
echo "Charles' wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/charles.addr) --testnet-magic $magic)"
echo ""
