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

parkingTag=11
pause=7
protocolParams=$assets/protocol.json
unitDatum=$assets/unit.cbor
stateA=$assets/datumRollupA.cbor
stateB=$assets/datumRollupB.cbor
rollupRedeemerA=$assets/redeemerRollupA.cbor
rollupValue=3000000

rollupScript=$(cardano-cli transaction txid --tx-file "$keypath/parkedScript.tx")#0

#-------------------------------- :rollup initial transfer: -------------------------------

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
in2=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[1]')

echo ""
echo "Testing pair of transactions..."
echo ""

echo "Building first Tx..."
echo ""

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/rollup.addr) + $rollupValue lovelace" \
  --tx-out-inline-datum-cbor-file $stateA \
  --change-address $(cat $keypath/alice.addr) \
  --out-file $keypath/rollupUpdateA.txbody

echo ""
echo "Signing first Tx..."
echo ""

cardano-cli conway transaction sign \
  --testnet-magic 4 \
  --tx-body-file $keypath/rollupUpdateA.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/rollupUpdateA.tx

echo "Building second Tx..."
echo ""

inA=$(cardano-cli transaction txid --tx-file "$keypath/rollupUpdateA.tx")#0

total2=1412298591
fee=3000000
collateralVal=5000000
change=$((total2 - $fee))
collateralExcess=$(($total2 - $collateralVal))

cardano-cli conway transaction build-raw \
  --tx-in $in2 \
  --tx-in $inA \
  --spending-tx-in-reference $rollupScript \
  --spending-plutus-script-v3 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-cbor-file $rollupRedeemerA \
  --spending-reference-tx-in-execution-units "(5000000000, 3000000)" \
  --tx-in-collateral $in2 \
  --tx-out-return-collateral "$(cat $keypath/alice.addr) + $collateralExcess lovelace" \
  --tx-total-collateral $collateralVal \
  --tx-out "$(cat $keypath/rollup.addr) + $rollupValue lovelace" \
  --tx-out-inline-datum-cbor-file $stateB \
  --tx-out "$(cat $keypath/alice.addr) + $change lovelace" \
  --fee $fee \
  --protocol-params-file $protocolParams \
  --out-file $keypath/rollupUpdateB.txbody

echo "Done building second Tx."
echo ""

echo "Signing second Tx..."
echo ""

cardano-cli conway transaction sign \
  --testnet-magic 4 \
  --tx-body-file $keypath/rollupUpdateB.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/rollupUpdateB.tx

echo "Submitting both Txs..."
echo ""

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file $keypath/rollupUpdateA.tx

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file $keypath/rollupUpdateB.tx



# rollupTx=$(cardano-cli transaction txid --tx-file "$keypath/rollupUpdate.tx")
# rollupOut=$rollupTx#0
# while true; do
#     txOnChain=$(cardano-cli query utxo --address $(cat ./keys/rollup.addr) --testnet-magic 4 --out-file /dev/stdout | jq -r --arg key "$rollupOut" 'has($key) | tostring')
#     if [ $txOnChain == "false" ]; then
# 	echo "Waiting to see initial rollup tx onchain..."
# 	sleep $pause
#     else
# 	echo ""
# 	echo "Transaction Id: $rollupTx"
# 	break
#     fi
# done

# echo ""
# echo "Initialization completed."
# echo ""

