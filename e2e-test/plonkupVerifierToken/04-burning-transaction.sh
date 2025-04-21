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
echo "bob burning tokens."
echo ""

in1=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic --out-file /dev/stdout | jq -r 'to_entries | map(select(.value.value | keys | length > 1)) | .[0].key')
in2=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic --out-file /dev/stdout | jq -r 'to_entries | map(select((.value.value | keys | length) == 1 and .value.value.lovelace > 10000000)) | .[0].key')
collateral=$in2

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic)"
echo ""

cabal run zkfold-cli token burning \
    --path-to-gycoreconfig $path \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-address "$keypath/burning-transaction.tx" \
    --tx-in $in1 \
    --tx-in $in2 \
    --tx-in $forwardingMintIn \
    --tx-out "$(cat $keypath/zkfold-main.addr)" \
    --tx-id "$keypath/forwardingMint.tx" \
    --tx-id "$keypath/plonkupVerifierToken.tx" \
    --signing-key-file "$keypath/someone.skey"

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic)"
echo ""
