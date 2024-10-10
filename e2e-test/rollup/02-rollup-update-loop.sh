#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets

unitDatum=$assets/unit.cbor
newStateA=$assets/datumRollupA.cbor
newStateB=$assets/datumRollupB.cbor
redeemerRollupA=$assets/redeemerRollupA.cbor
redeemerRollupB=$assets/redeemerRollupB.cbor
rollupValue=3000000

rollupScript=$(cardano-cli transaction txid --tx-file "$keypath/parkedScript.tx")#0

period=7
loop=true

while $loop; do
    loop=$(cat $keypath/rollup-loop.flag)

    in2=$(cardano-cli transaction txid --tx-file "$keypath/rollupUpdate.tx")
    in2A=$in2#0
    in2B=$in2#1
    txOnChain=$(cardano-cli query utxo --address $(cat ./keys/rollup.addr) --testnet-magic 4 --out-file /dev/stdout | jq -r --arg key "$in2A" 'has($key) | tostring')

    if [ $txOnChain == "false" ]; then
	echo "Waiting to see rollup tx onchain..."
	sleep $period
    else
	echo ""
	echo "Starting next rollup update..."
	echo ""

	cabal run rollup-update-loop

	in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
	collateral=$in1

	cardano-cli conway transaction build \
	  --testnet-magic 4 \
	  --tx-in $in1 \
	  --tx-in $in2A \
	  --spending-tx-in-reference $rollupScript \
	  --spending-plutus-script-v3 \
	  --spending-reference-tx-in-inline-datum-present \
	  --spending-reference-tx-in-redeemer-cbor-file $redeemerRollupA \
	  --tx-in $in2B \
	  --spending-tx-in-reference $rollupScript \
	  --spending-plutus-script-v3 \
	  --spending-reference-tx-in-inline-datum-present \
	  --spending-reference-tx-in-redeemer-cbor-file $redeemerRollupB \
	  --tx-in-collateral $collateral \
	  --tx-out "$(cat $keypath/rollup.addr) + $rollupValue lovelace" \
	  --tx-out-inline-datum-cbor-file $newStateA \
	  --tx-out "$(cat $keypath/rollup.addr) + $rollupValue lovelace" \
	  --tx-out-inline-datum-cbor-file $newStateB \
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
	echo "Rollup-update batch completed."
	echo "Transaction Id: $(cardano-cli transaction txid --tx-file $keypath/rollupUpdate.tx)"
	echo ""

	mv $assets/nextRedeemerRollupA.cbor $assets/redeemerRollupA.cbor
	mv $assets/nextRedeemerRollupA.json $assets/redeemerRollupA.json

	mv $assets/nextRedeemerRollupB.cbor $assets/redeemerRollupB.cbor
	mv $assets/nextRedeemerRollupB.json $assets/redeemerRollupB.json
    fi
done
