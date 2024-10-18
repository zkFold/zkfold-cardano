#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets

protocolParams=$assets/protocol.json
unitDatum=$assets/unit.cbor
stateA=$assets/datumRollupA.cbor
stateB=$assets/datumRollupB.cbor
rollupRedeemerA=$assets/redeemerRollupA.cbor
rollupRedeemerB=$assets/redeemerRollupB.cbor

rollupScript=$(cardano-cli transaction txid --tx-file "$keypath/parkedScript.tx")#0
rollupScriptFile=$assets/rollup.plutus
rollupVerifierSize=$(stat -c%s "$rollupScriptFile")

#-------------------------------- :numeric parameters: -------------------------------

pause=7  # Wait time (in seconds) before querying blockchain
rollupValue=3000000  # Value (in lovelaces) to be transfered with each rollup

# ----- Linear Model for Exec Units -----
# (cpu steps of Tx B) = (cpu steps of Tx A) + incCpu
# (memory of Tx B)    = (memory of Tx A) + incMem

incCpu=12641403
incMem=30129

#-------------------------------- :rollup loop: -------------------------------

loop=true
printf "$loop" > $keypath/rollup-loop.flag

while $loop; do
    loop=$(cat $keypath/rollup-loop.flag)

    inRB=$(cardano-cli transaction txid --tx-file "$keypath/rollupOutB.tx")#0
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/rollup.addr) --testnet-magic 4 --out-file /dev/stdout | jq -r --arg key "$inRB" 'has($key) | tostring')

    if [ $txOnChain == "false" ]; then
	echo "Waiting to see rollup tx onchain..."
	sleep $pause
    else
	echo ""
	echo "Starting next rollup update..."
	echo ""

	cabal run rollup-update-loop

	echo "Building first Tx..."
	echo ""

	in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

	# Log execution units for first Tx
	cardano-cli conway transaction build \
	  --testnet-magic 4 \
	  --tx-in $in1 \
	  --tx-in $inRB \
	  --spending-tx-in-reference $rollupScript \
	  --spending-plutus-script-v3 \
	  --spending-reference-tx-in-inline-datum-present \
	  --spending-reference-tx-in-redeemer-cbor-file $rollupRedeemerA \
	  --tx-in-collateral $in1 \
	  --tx-out "$(cat $keypath/rollup.addr) + $rollupValue lovelace" \
	  --tx-out-inline-datum-cbor-file $stateA \
	  --change-address $(cat $keypath/alice.addr) \
	  --calculate-plutus-script-cost $keypath/exec-units-A.log > /dev/null

	execCpuA=$(cat $keypath/exec-units-A.log | jq -r '.[0].executionUnits.steps')
	execMemA=$(cat $keypath/exec-units-A.log | jq -r '.[0].executionUnits.memory')

	# Build first Tx
	cardano-cli conway transaction build \
	  --testnet-magic 4 \
	  --tx-in $in1 \
	  --tx-in $inRB \
	  --spending-tx-in-reference $rollupScript \
	  --spending-plutus-script-v3 \
	  --spending-reference-tx-in-inline-datum-present \
	  --spending-reference-tx-in-redeemer-cbor-file $rollupRedeemerA \
	  --tx-in-collateral $in1 \
	  --tx-out "$(cat $keypath/rollup.addr) + $rollupValue lovelace" \
	  --tx-out-inline-datum-cbor-file $stateA \
	  --change-address $(cat $keypath/alice.addr) \
	  --out-file $keypath/rollupOutA.txbody

	echo ""
	echo "Signing first Tx..."
	echo ""

	cardano-cli conway transaction sign \
	  --testnet-magic 4 \
	  --tx-body-file $keypath/rollupOutA.txbody \
	  --signing-key-file $keypath/alice.skey \
	  --out-file $keypath/rollupOutA.tx

	echo "Building second Tx..."
	echo ""

	in2=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[1]')
	inRA=$(cardano-cli transaction txid --tx-file "$keypath/rollupOutA.tx")#0

	total2=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file /dev/stdout | jq -r '. | to_entries[1].value.value.lovelace')
	totalUtxoVal=$(($total2 + $rollupValue))
	collateralVal=5000000
	collateralExcess=$(($total2 - $collateralVal))

	updateLength=$(cat "$assets/last-update-length.log")
	execUnits="($((execCpuA + incCpu)), $((execMemA + incMem)))"

	# Build second Tx
	cardano-cli conway transaction build-estimate \
	  --shelley-key-witnesses 1 \
	  --protocol-params-file $protocolParams \
	  --total-utxo-value $totalUtxoVal \
	  --tx-in $in2 \
	  --tx-in $inRA \
	  --spending-tx-in-reference $rollupScript \
	  --spending-plutus-script-v3 \
	  --spending-reference-tx-in-inline-datum-present \
	  --spending-reference-tx-in-redeemer-cbor-file $rollupRedeemerB \
	  --spending-reference-tx-in-execution-units "$execUnits" \
	  --tx-in-collateral $in2 \
	  --tx-out-return-collateral "$(cat $keypath/alice.addr) + $collateralExcess lovelace" \
	  --tx-out "$(cat $keypath/rollup.addr) + $rollupValue lovelace" \
	  --tx-out-inline-datum-cbor-file $stateB \
	  --change-address $(cat $keypath/alice.addr) \
	  --reference-script-size $rollupVerifierSize \
	  --out-file $keypath/rollupOutB.txbody

	fee2=$(cardano-cli conway transaction calculate-min-fee \
		 --tx-body-file $keypath/rollupOutB.txbody \
		 --protocol-params-file $protocolParams \
		 --witness-count 1 \
		 --reference-script-size $rollupVerifierSize | sed 's/ .*//')

	echo "Estimated transaction fee: Coin $fee2"
	echo ""

	echo "Signing second Tx..."
	echo ""

	cardano-cli conway transaction sign \
	  --testnet-magic 4 \
	  --tx-body-file $keypath/rollupOutB.txbody \
	  --signing-key-file $keypath/alice.skey \
	  --out-file $keypath/nextRollupOutB.tx

	echo "Submitting both Txs..."
	echo ""

	cardano-cli conway transaction submit \
	    --testnet-magic 4 \
	    --tx-file $keypath/rollupOutA.tx

	cardano-cli conway transaction submit \
	    --testnet-magic 4 \
	    --tx-file $keypath/nextRollupOutB.tx

	echo ""
	echo "Rollup-update batch completed.  Last update length: $updateLength."
	echo "Transaction Id: $(cardano-cli transaction txid --tx-file $keypath/rollupOutA.tx)"
	echo "Transaction Id: $(cardano-cli transaction txid --tx-file $keypath/nextRollupOutB.tx)"
	echo ""

	mv $keypath/nextRollupOutB.tx $keypath/rollupOutB.tx
	mv $assets/nextRedeemerRollupA.cbor $assets/redeemerRollupA.cbor
	mv $assets/nextRedeemerRollupA.json $assets/redeemerRollupA.json
    fi
done
