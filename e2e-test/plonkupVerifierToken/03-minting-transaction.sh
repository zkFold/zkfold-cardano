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
echo "alice minting tokens for bob."
echo ""

# cabal run zkfold-cli token minting ...

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic)"
echo ""
