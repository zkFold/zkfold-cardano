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
stateB=$assets/datumRollupB.cbor
rollupRedeemerB=$assets/redeemerRollupB.cbor

rollupScript=$(cardano-cli transaction txid --tx-file "$keypath/parkedScript.tx")#0
rollupScriptFile=$assets/rollup.plutus
rollupVerifierSize=$(stat -c%s "$rollupScriptFile")

#-------------------------------- :numeric parameters: -------------------------------

pause=7
rollupValue=3000000  # Value (in lovelaces) to be transfered with each rollup

cpuA=4252791294
cpuB=8427602
cpuC=28000

memA=2665606
memB=20452
memC=20

#-------------------------------- :second transaction: -------------------------------

echo "Building second Tx..."
echo ""

in2=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[1]')
inRA=$(cardano-cli transaction txid --tx-file "$keypath/rollupOutA.tx")#0

total2=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file /dev/stdout | jq -r '. | to_entries[1].value.value.lovelace')
totalUtxoVal=$(($total2 + $rollupValue))
collateralVal=5000000
collateralExcess=$(($total2 - $collateralVal))

updateLength=$(cat "$assets/last-update-length.log")
execUnits="($((cpuA + cpuB * updateLength + cpuC * updateLength * updateLength)), $((memA + memB * updateLength + memC * updateLength * updateLength)))"

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

echo "Submitting second Txs..."
echo ""

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file $keypath/nextRollupOutB.tx

echo ""
echo "Rollup-update batch completed.  Last update length: $updateLength."
echo "Transaction Id: $(cardano-cli transaction txid --tx-file $keypath/rollupOutA.tx)"
echo "Transaction Id: $(cardano-cli transaction txid --tx-file $keypath/nextRollupOutB.tx)"
echo ""