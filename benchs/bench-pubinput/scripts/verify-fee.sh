#! /bin/bash

set -e
set -u
set -o pipefail

keypath="./scripts/keys"
assetspath="../../assets"

# payment=$(jq -r '.payment' assets/config.json)
# validRangeSlots=$(jq -r '.["valid-range-slots"]' assets/config.json)

in1=$(cardano-cli conway query utxo --address $(cat $keypath/symbolicVerifierBench1.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
# val1=$(cardano-cli conway query utxo --address $(cat $keypath/pubInput.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r --arg key "$in1" '.[$key].value.lovelace')
in2=$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

# currentSlot=$(cardano-cli conway query tip --testnet-magic 4 | jq -r '.slot')
# uptoSlot=$(expr $currentSlot + 200)

# cardano-cli conway transaction calculate-min-required-utxo \
#   --protocol-params-file $assetspath/protocol.json \
#   --tx-out "$(cat $keypath/alwaysSucceeds.addr) + 1 lovelace"
#   --tx-out-inline-datum-cbor-file $assetspath/input-data.cbor

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
  --calculate-plutus-script-cost $keypath/cost2.txbody
