#! /bin/bash

set -e
set -u
set -o pipefail

keypath="./scripts/keys"
assetspath="../../assets"

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

datum=$assetspath/unit.cbor

# cardano-cli conway transaction build \
#     --testnet-magic 4 \
#     --change-address "$(cat $keypath/alice.addr)" \
#     --out-file "$keypath/tx.body" \
#     --tx-in $in1 \
#     --tx-out "$(cat $keypath/pubInput.addr) + 10000000 lovelace" \
#     --tx-out-inline-datum-cbor-file $datum

# cardano-cli conway transaction sign \
#     --testnet-magic 4 \
#     --tx-body-file "$keypath/tx.body" \
#     --signing-key-file "$keypath/alice.skey" \
#     --out-file "$keypath/tx.signed"

# cardano-cli conway transaction submit \
#     --testnet-magic 4 \
#     --tx-file "$keypath/tx.signed"

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/alice.addr)" \
    --out-file "$keypath/tx.body" \
    --tx-in $in1 \
    --tx-out "$(cat $keypath/symbolicVerifierBench1.addr) + 10000000 lovelace" \
    --tx-out-inline-datum-cbor-file $datum

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/tx.body" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/tx.signed"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/tx.signed"
