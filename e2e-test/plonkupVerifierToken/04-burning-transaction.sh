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
echo "bob burning tokens."
echo ""

in1=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic --out-file /dev/stdout | jq -r 'to_entries | map(select(.value.value | keys | length > 1)) | .[0].key')
in2=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic --out-file /dev/stdout | jq -r 'to_entries | map(select((.value.value | keys | length) == 1 and .value.value.lovelace > 10000000)) | .[0].key')
collateral=$in2

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic)"
echo ""

# cabal run zkfold-cli token burning ...

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic)"
echo ""
