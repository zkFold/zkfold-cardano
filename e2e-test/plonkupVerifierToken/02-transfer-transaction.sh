#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

previewmagic=2
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

cabal run zkfold-cli token transfer \
    --path-to-gycoreconfig $path \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-address "$keypath/plonkupVerifierToken-transfer.tx" \
    --tx-in $in1 \
    --signing-key-file "$keypath/someone.skey"

echo "forwardingMint address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/forwardingMint.addr) --testnet-magic $magic)"
echo ""
