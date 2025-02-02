#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

sanchomagic=4
keypath=./plonkupVerifierToken/keys
privpath=./plonkupVerifierToken/priv
assets=../assets

magic=$(cat $privpath/testnet.flag)

echo ""
echo "charles wants to create an address with lock reward."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/charles.addr) --testnet-magic $magic --out-file  /dev/stdout | jq -r 'keys[0]')

echo "charles address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/charles.addr) --testnet-magic $magic)"
echo ""

# cabal run zkfold-cli token transfer ...

echo "forwardingMint address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/forwardingMint.addr) --testnet-magic $magic)"
echo ""
