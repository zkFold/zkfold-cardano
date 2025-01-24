#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./plonkupVerifierToken/keys
privpath=./plonkupVerifierToken/priv

magic=$(cat $privpath/testnet.flag)

echo ""
echo "someone address: $(cat $keypath/someone.addr)"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/someone.addr) --testnet-magic $magic)"
echo ""
echo "charles address: $(cat $keypath/charles.addr)"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/charles.addr) --testnet-magic $magic)"
echo ""
echo "alice address: $(cat $keypath/alice.addr)"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $magic)"
echo ""
echo "bob address: $(cat $keypath/bob.addr)"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic)"
echo ""
echo "zkfold-main address: $(cat $keypath/zkfold-main.addr)"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/zkfold-main.addr) --testnet-magic $magic)"
echo ""
