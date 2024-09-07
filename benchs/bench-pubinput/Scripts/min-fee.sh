#! /bin/bash

set -e
set -u
set -o pipefail

keypath="./Scripts/keys"
assetspath="../../assets"

# payment=$(jq -r '.payment' assets/config.json)
# validRangeSlots=$(jq -r '.["valid-range-slots"]' assets/config.json)

in1=$(cardano-cli conway query utxo --address $(cat $keypath/pubInput.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
val1=$(cardano-cli conway query utxo --address $(cat $keypath/pubInput.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r --arg key "$in1" '.[$key].value.lovelace')
in2=$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

currentSlot=$(cardano-cli conway query tip --testnet-magic 4 | jq -r '.slot')
uptoSlot=$(expr $currentSlot + 200)

# cardano-cli conway transaction calculate-min-required-utxo \
#   --protocol-params-file $assetspath/protocol.json \
#   --tx-out "$(cat $keypath/alwaysSucceeds.addr) + 1 lovelace"
#   --tx-out-inline-datum-cbor-file $assetspath/input-data.cbor

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
  --calculate-plutus-script-cost $keypath/cost.txbody

# --out-file $keypath/test.txbody

# --tx-in-execution-units "(1000000, 1000)" \
# --fee 0 ?
# --protocol-params-file FILE

  # --tx-in $in2 \
  # --tx-out $(cat $keypath/alice.addr)+10000000 \
  # --tx-out $(cat $keypath/alice.addr)+$(expr $val1 - 1000000) \
  # --invalid-hereafter $uptoSlot \

exit 1
minFee=$(cardano-cli conway transaction calculate-min-fee \
  --tx-body-file $keypath/tx.draft \
  --protocol-params-file $assetspath/protocol.json \
  --witness-count 1 \
  --testnet-magic 4 | sed 's/ .*//')

echo "Min Fee:"
echo "$minFee"

# cardano-cli conway transaction build-raw \
#   --tx-in $in1 \
#   --tx-out $(cat keys/bob.addr)+$payment \
#   --tx-out $(cat keys/alice.addr)+$(expr $val1 - $payment - $minFee) \
#   --invalid-hereafter $uptoSlot \
#   --fee $minFee \
#   --out-file keys/tx.raw

# cardano-cli conway transaction sign \
#   --tx-body-file keys/tx.raw \
#   --signing-key-file keys/alice.skey \
#   --testnet-magic 4 \
#   --out-file keys/tx.signed

# cardano-cli conway transaction submit \
#   --tx-file keys/tx.signed \
#   --testnet-magic 4
