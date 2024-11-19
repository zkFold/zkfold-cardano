#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

sanchomagic=4
assets=../assets
keypath=./symbolic-balancing/keys
privpath=./symbolic-balancing/priv

mN=$(cat $privpath/testnet.flag)

mkdir -p $assets

# Wait time (in seconds) before querying blockchain
if [ $mN == $sanchomagic ]; then
    pause=7
else
    pause=4
fi

unitDatum=$assets/unit.cbor

echo ""
echo "Initialization..."
echo ""

#-------------------------------- :protocol parameters: ------------------------------

cardano-cli conway query protocol-parameters \
  --testnet-magic $mN \
  --out-file $assets/protocol.json

#------------------------------- :create scripts: ------------------------------

cabal run balancing-init-transaction

#-------------------------------- :symbolic setup: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/symbolic.plutus" \
    --out-file "$keypath/symbolic.addr" \
    --testnet-magic $mN

#-------------------------------- :parkingSpot setup: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/parkingSpot.plutus" \
    --out-file "$keypath/parkingSpot.addr" \
    --testnet-magic $mN

#-------------------------------- :park symbolic script: -------------------------------

echo "Parking 'symbolic.plutus'..."

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file  /dev/stdout | jq -r 'to_entries | map(select(.value.value.lovelace > 25000000)) | .[0].key')

parkScriptMinCost=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $assets/protocol.json \
  --tx-out $(cat $keypath/parkingSpot.addr)+0 \
  --tx-out-reference-script-file "$assets/symbolic.plutus" | sed 's/^[^ ]* //')

cardano-cli conway transaction build \
  --testnet-magic $mN \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/parkingSpot.addr) + $parkScriptMinCost lovelace " \
  --tx-out-reference-script-file $assets/symbolic.plutus \
  --change-address $(cat $keypath/alice.addr) \
  --out-file $keypath/parkedScript.txbody

cardano-cli conway transaction sign \
  --testnet-magic $mN \
  --tx-body-file $keypath/parkedScript.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/parkedScript.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/parkedScript.tx

parkedTx=$(cardano-cli transaction txid --tx-file "$keypath/parkedScript.tx")
parkedOut=$parkedTx#0

while true; do
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/parkingSpot.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$parkedOut" 'has($key) | tostring')
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


#-------------------------------- :symbolic initial transfer: -------------------------------

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file  /dev/stdout | jq -r 'to_entries | map(select(.value.value.lovelace > 25000000)) | .[0].key')

echo "Initial transfer to symbolic..."
echo ""

cardano-cli conway transaction build \
  --testnet-magic $mN \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/symbolic.addr) + 10000000 lovelace" \
  --tx-out-inline-datum-cbor-file $unitDatum \
  --change-address $(cat $keypath/alice.addr) \
  --out-file $keypath/fundSymb.txbody

cardano-cli conway transaction sign \
  --testnet-magic $mN \
  --tx-body-file $keypath/fundSymb.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/fundSymb.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/fundSymb.tx

symbolicTx=$(cardano-cli transaction txid --tx-file "$keypath/fundSymb.tx")
symbolicOut=$symbolicTx#0
while true; do
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/symbolic.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$symbolicOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see initial transfer to symbolic..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $symbolicTx"
	break
    fi
done

cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file $assets/utxo1.json
cardano-cli conway query utxo --address $(cat $keypath/symbolic.addr) --testnet-magic $mN --out-file $assets/utxo2.json

echo ""
echo "Initialization completed."
echo ""
