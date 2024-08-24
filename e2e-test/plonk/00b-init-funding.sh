#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys

echo "Fund alice, bob and charles"
echo "(Assuming someone has been funded from Faucet.)"

#----------------------------------- :alice: -----------------------------------

in1=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/alice.addr) + 2000000000" \
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
echo "Pausing for 35 seconds..."
echo ""
sleep 35

echo "$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"

#------------------------------------ :bob: ------------------------------------

in2=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
  --tx-in $in2 \
  --tx-out "$(cat $keypath/bob.addr) + 2000000000" \
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
echo "Pausing for 35 seconds..."
echo ""
sleep 35

echo "$(cardano-cli conway query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"

#----------------------------------- :charles: -----------------------------------

in3=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
  --tx-in $in3 \
  --tx-out "$(cat $keypath/charles.addr) + 2000000000" \
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
echo "Pausing for 35 seconds..."
echo ""
sleep 35

echo "$(cardano-cli conway query utxo --address $(cat $keypath/charles.addr) --testnet-magic 4)"
