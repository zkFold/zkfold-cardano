#! /bin/bash

set -e
set -u
set -o pipefail

# SUMMARY: Computing cost for various sripts.

keypath="./scripts/keys"
assetspath="../../assets"

in1=$(jq -r '.inputs[0]' $assetspath/inputs.json)  # Bob
in2=$(jq -r '.inputs[1]' $assetspath/inputs.json)  # Unit

#----------------------------- :'pubInput': -----------------------------

in1=$(cardano-cli conway query utxo --address $(cat $keypath/pubInput.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
in2=$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --tx-in $in1 \
  --tx-in-script-file $assetspath/pubInput.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-cbor-file $assetspath/input-data.cbor \
  --tx-in $in2 \
  --tx-in-collateral $in2 \
  --tx-out "$(cat $keypath/alice.addr) + 10000000 lovelace" \
  --change-address "$(cat $keypath/alice.addr)" \
  --calculate-plutus-script-cost $keypath/pubInput.cost

#---------------------- :'symbolicVerifierBench1': ----------------------

in1=$(cardano-cli conway query utxo --address $(cat $keypath/symbolicVerifierBench1.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
in2=$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --tx-in $in1 \
  --tx-in-script-file $assetspath/symbolicVerifierBench1.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-cbor-file $assetspath/genericRedeemer.cbor \
  --tx-in $in2 \
  --tx-in-collateral $in2 \
  --tx-out "$(cat $keypath/alice.addr) + 10000000 lovelace" \
  --change-address "$(cat $keypath/alice.addr)" \
  --calculate-plutus-script-cost $keypath/symbolicVerifierBench1.cost

#-------------------------- :'alwaysSucceds': ---------------------------

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --tx-in $in1 \
  --tx-in $in2 \
  --tx-in-script-file $assetspath/alwaysSucceeds.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-cbor-file $assetspath/unit.cbor \
  --tx-in-collateral $in1 \
  --tx-out "$(cat $keypath/alice.addr) + 1000000 lovelace" \
  --tx-out "$(cat $keypath/alwaysSucceeds.addr) + 9000000 lovelace" \
  --tx-out-inline-datum-cbor-file $assetspath/unit.cbor \
  --change-address "$(cat $keypath/bob.addr)" \
  --calculate-plutus-script-cost $keypath/alwaysSucceeds.cost
