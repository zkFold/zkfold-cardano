#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets

parkingTag=17
unitDatum=$assets/unit.cbor
initialState=$assets/datumRollup.cbor
rollupValue=3000000

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo ""
echo "Initialization..."
echo ""

#-------------------------------- :protocol parameters: ------------------------------

cardano-cli conway query protocol-parameters \
  --testnet-magic 4 \
  --out-file $assets/protocol.json

#------------------------------- :create scripts: ------------------------------

cabal run rollup-init-transaction -- $parkingTag

#-------------------------------- :rollup setup: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/rollup.plutus" \
    --out-file "$keypath/rollup.addr" \
    --testnet-magic 4

#-------------------------------- :parkingSpot setup: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/parkingSpot.plutus" \
    --out-file "$keypath/parkingSpot.addr" \
    --testnet-magic 4

#-------------------------------- :park rollup script: -------------------------------

echo "Parking 'rollup.plutus'..."

parkScriptMinCost=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $assets/protocol.json \
  --tx-out $(cat $keypath/parkingSpot.addr)+0 \
  --tx-out-reference-script-file "$assets/rollup.plutus" | sed 's/^[^ ]* //')

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/parkingSpot.addr) + $parkScriptMinCost lovelace " \
  --tx-out-reference-script-file $assets/rollup.plutus \
  --change-address $(cat $keypath/alice.addr) \
  --out-file $keypath/parkedScript.txbody

cardano-cli conway transaction sign \
  --testnet-magic 4 \
  --tx-body-file $keypath/parkedScript.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/parkedScript.tx

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file $keypath/parkedScript.tx

echo ""
echo "Pausing for 75 seconds..."
sleep 75

echo ""
echo "Transaction Id: $(cardano-cli transaction txid --tx-file $keypath/parkedScript.tx)"
echo ""

#-------------------------------- :rollup initial transfer: -------------------------------

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo "Rollup initial transfer..."

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/rollup.addr) + $rollupValue lovelace" \
  --tx-out-inline-datum-cbor-file $initialState \
  --change-address $(cat $keypath/alice.addr) \
  --out-file $keypath/rollupUpdate.txbody

cardano-cli conway transaction sign \
  --testnet-magic 4 \
  --tx-body-file $keypath/rollupUpdate.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/rollupUpdate.tx

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file $keypath/rollupUpdate.tx

echo ""
echo "Pausing for 50 seconds..."
sleep 50

echo ""
echo "Transaction Id: $(cardano-cli transaction txid --tx-file $keypath/rollupUpdate.tx)"
echo ""
