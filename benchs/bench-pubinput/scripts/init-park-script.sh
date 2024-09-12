#! /bin/bash

set -e
set -u
set -o pipefail

keypath="./scripts/keys"
assetspath="../../assets"

unitDatum=$assetspath/unit.cbor

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

#-------------------------- :parking 'symbolicVerifier': ---------------------------

# We park 'symbolicVerifier' on script 'alwaysSucceeds', as an example.

transferScriptUtxo=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $assetspath/protocol.json \
  --tx-out $(cat $keypath/alwaysSucceeds.addr)+0 \
  --tx-out-inline-datum-cbor-file $unitDatum \
  --tx-out-reference-script-file "$assetspath/symbolicVerifier.plutus" | sed 's/^[^ ]* //')

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --change-address "$(cat $keypath/alice.addr)" \
  --out-file "$keypath/transferScript.txbody" \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/alwaysSucceeds.addr) + $transferScriptUtxo lovelace" \
  --tx-out-inline-datum-cbor-file $unitDatum \
  --tx-out-reference-script-file "$assetspath/symbolicVerifier.plutus"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/transferScript.txbody" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/transferScript.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/transferScript.tx"

echo "transaction id: $(cardano-cli conway transaction txid --tx-file "$keypath/transferScript.tx")"
