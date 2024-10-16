#! /bin/bash

set -e
set -u
set -o pipefail

keypath="./scripts/keys"
assetspath="../../assets"
pause=50

unitDatum=$assetspath/unit.cbor
symbolicDatum=$assetspath/datumSymbolic.cbor

#---------------------- :funding 'symbolicVerifier': -----------------------

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/alice.addr)" \
    --out-file "$keypath/fundSymbolic.txbody" \
    --tx-in $in1 \
    --tx-out "$(cat $keypath/symbolicVerifier.addr) + 10000000 lovelace" \
    --tx-out-inline-datum-cbor-file $unitDatum

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/fundSymbolic.txbody" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/fundSymbolic.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/fundSymbolic.tx"

echo ""
echo "Pausing for $pause seconds..."
echo ""
sleep $pause
