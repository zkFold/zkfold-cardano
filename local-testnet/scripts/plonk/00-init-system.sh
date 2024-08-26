#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath0=./example/utxo-keys
keypath=./example/keys

echo "Create someone, zkfold-setup, alice and bob..."

mkdir -p $keypath

#---------------------------------- :someone: ----------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/someone.vkey \
  --signing-key-file $keypath/someone.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/someone.vkey \
  --out-file $keypath/someone.addr \
  --testnet-magic 42

#----------------------------------- :charles: -----------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/charles.vkey \
  --signing-key-file $keypath/charles.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/charles.vkey \
  --out-file $keypath/charles.addr \
  --testnet-magic 42

#----------------------------------- :alice: -----------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/alice.vkey \
  --signing-key-file $keypath/alice.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/alice.vkey \
  --out-file $keypath/alice.addr \
  --testnet-magic 42

#------------------------------------ :bob: ------------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/bob.vkey \
  --signing-key-file $keypath/bob.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/bob.vkey \
  --out-file $keypath/bob.addr \
  --testnet-magic 42

#-------------------------------- :zkfold-main: --------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/zkfold-main.vkey \
  --signing-key-file $keypath/zkfold-main.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/zkfold-main.vkey \
  --out-file $keypath/zkfold-main.addr \
  --testnet-magic 42

#-------------------------------------------------------------------------------

echo "Wallets created."
echo "Funding someone, alice, bob and charles..."

#----------------------------------- :utxo1: -----------------------------------

cardano-cli conway address build \
  --payment-verification-key-file $keypath0/utxo1.vkey \
  --out-file $keypath0/utxo1.addr \
  --testnet-magic 42

#----------------------------------- :funding: -----------------------------------

in1=$(cardano-cli query utxo --address $(cat $keypath0/utxo1.addr) --testnet-magic 42 --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/someone.addr) + 500000000" \
  --tx-out "$(cat $keypath/someone.addr) + 500000000" \
  --tx-out "$(cat $keypath/alice.addr) + 500000000" \
  --tx-out "$(cat $keypath/bob.addr) + 500000000" \
  --tx-out "$(cat $keypath/charles.addr) + 500000000" \
  --change-address $(cat $keypath0/utxo1.addr) \
  --out-file $keypath/tx.body \
  --testnet-magic 42

cardano-cli conway transaction sign \
  --tx-body-file $keypath/tx.body \
  --signing-key-file $keypath0/utxo1.skey \
  --out-file $keypath/tx.signed \
  --testnet-magic 42

cardano-cli conway transaction submit \
  --tx-file $keypath/tx.signed \
  --testnet-magic 42

echo ""
echo "Pausing for 5 seconds..."
echo ""
sleep 5

echo "Someone's wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/someone.addr) --testnet-magic 42)"
echo ""
echo "Alice's wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic 42)"
echo ""
echo "Bob's wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/bob.addr) --testnet-magic 42)"
echo ""
echo "Charles' wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/charles.addr) --testnet-magic 42)"
echo ""
