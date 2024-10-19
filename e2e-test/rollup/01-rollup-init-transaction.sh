#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets

mkdir -p $assets
mkdir -p $keypath

parkingTag=54
pause=7
unitDatum=$assets/unit.cbor
stateA=$assets/datumRollupA.cbor
stateB=$assets/datumRollupB.cbor
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

parkedTx=$(cardano-cli transaction txid --tx-file "$keypath/parkedScript.tx")
parkedOut=$parkedTx#0

while true; do
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/parkingSpot.addr) --testnet-magic 4 --out-file /dev/stdout | jq -r --arg key "$parkedOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see script parked onchain..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $parkedTx"
	echo ""
	break
    fi
done

#-------------------------------- :rollup initial transfer: -------------------------------

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo "Transfering initial state..."
echo ""

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/rollup.addr) + $rollupValue lovelace" \
  --tx-out-inline-datum-cbor-file $stateA \
  --change-address $(cat $keypath/alice.addr) \
  --out-file $keypath/rollupOutB.txbody

cardano-cli conway transaction sign \
  --testnet-magic 4 \
  --tx-body-file $keypath/rollupOutB.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/rollupOutB.tx

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file $keypath/rollupOutB.tx

rollupTx=$(cardano-cli transaction txid --tx-file "$keypath/rollupOutB.tx")
rollupOut=$rollupTx#0
while true; do
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/rollup.addr) --testnet-magic 4 --out-file /dev/stdout | jq -r --arg key "$rollupOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see initial rollup tx onchain..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $rollupTx"
	break
    fi
done

echo ""
echo "Initialization completed."
echo ""
