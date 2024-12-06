#! /bin/bash

set -e
set -u
set -o pipefail

keypath="./scripts/keys"
assetspath="../../assets"
pause=75

unitDatum=$assetspath/unit.cbor
plonkDatum=$assetspath/datumPlonk.cbor

#-------------------------- :funding 'pubInput': ---------------------------

# Script 'pubInput' is used to estimate exec units for (blake2b_224) hashing the
# input data and converting to Integer.

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/alice.addr)" \
    --out-file "$keypath/tx.body" \
    --tx-in $in1 \
    --tx-out "$(cat $keypath/pubInput.addr) + 10000000 lovelace" \
    --tx-out-inline-datum-cbor-file $unitDatum

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/tx.body" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/tx.signed"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/tx.signed"

echo "\nPausing for $pause seconds...\n"
sleep $pause

#------------------- :funding 'symbolicVerifierBench1': --------------------

# Script 'symbolicVerifierBench1' is used to estimate exec units for the
# 'verify @PlonkPlutus' part of 'symbolicVerifier'.

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/alice.addr)" \
    --out-file "$keypath/tx.body" \
    --tx-in $in1 \
    --tx-out "$(cat $keypath/symbolicVerifierBench1.addr) + 10000000 lovelace" \
    --tx-out-inline-datum-cbor-file $unitDatum

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/tx.body" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/tx.signed"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/tx.signed"

echo "\nPausing for $pause seconds...\n"
sleep $pause

#---------------------- :funding 'symbolicVerifier': -----------------------

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/alice.addr)" \
    --out-file "$keypath/tx.body" \
    --tx-in $in1 \
    --tx-out "$(cat $keypath/symbolicVerifier.addr) + 10000000 lovelace" \
    --tx-out-inline-datum-cbor-file $unitDatum

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/tx.body" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/tx.signed"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/tx.signed"

echo "\nPausing for $pause seconds...\n"
sleep $pause

#---------------------- :funding 'alwaysSucceds': -----------------------

# Funding script 'alwaysSucceds' with an arbitrary datum, as an example.

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/alice.addr)" \
    --out-file "$keypath/loadDatum.txbody" \
    --tx-in $in1 \
    --tx-out "$(cat $keypath/alwaysSucceeds.addr) + 10000000 lovelace" \
    --tx-out-inline-datum-cbor-file $plonkDatum

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/loadDatum.txbody" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/loadDatum.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/loadDatum.tx"

echo "\nPausing for $pause seconds...\n"
sleep $pause
