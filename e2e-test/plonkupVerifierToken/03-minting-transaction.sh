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

cabal run zkfold-cli token minting \
    --path-to-gycoreconfig $path \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-address "$keypath/minting-transaction.tx" \
    --tx-in $in1 \
    --tx-out "$(cat $keypath/zkfold-main.addr)" \
    --tx-id "$keypath/forwardingMint.txbody" \
    --signing-key-file "$keypath/someone.skey"

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic)"
echo ""
