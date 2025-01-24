#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys

echo ""
echo "alice address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"
echo ""
echo "someone address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4)"
echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""
echo "plonkupVerifierTx address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/plonkupVerifierTx.addr) --testnet-magic 4)"
echo ""
echo "zkfold-setup address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/zkfold-setup.addr) --testnet-magic 4)"
echo ""
echo "zkfold-main address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/zkfold-main.addr) --testnet-magic 4)"
echo ""
