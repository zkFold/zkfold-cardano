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
echo "someone wants to create an address with scripts."
echo ""

in1=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic $magic --out-file  /dev/stdout | jq -r 'keys[0]')
in2=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic $magic --out-file  /dev/stdout | jq -r 'keys[1]')

echo "someone address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic $magic)"
echo ""

# cabal run zkfold-cli token init \
#     --path-to-gycoreconfig $GYCORECONFIG_PATH \
#     --change-address "$(cat $keypath/someone.addr)" \
#     --out-address "$keypath/forwardingMint.tx" \
#     --out-address "$keypath/plonkupVerifierToken.tx" \
#     --tx-in $in1 \
#     --tx-out "$(cat $keypath/zkfold-main.addr)" \
#     --signing-key-file "$keypath/someone.skey"

cabal run zkfold-cli -- token-init \
    --path-to-gycoreconfig $GYCORECONFIG_PATH \
    --tx-in $in1 \
    --signing-key-file "$keypath/someone.skey" \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-address "$(cat $keypath/zkfold-main.addr)" \
    --tx-out "$keypath/parkedScripts.tx"

echo "zkfold-main address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/zkfold-main.addr) --testnet-magic $magic)"
echo ""
