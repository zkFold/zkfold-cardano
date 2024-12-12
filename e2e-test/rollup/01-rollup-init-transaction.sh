#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

sanchomagic=4
assets=../assets
keypath=./rollup/keys
privpath=./rollup/priv

mN=$(cat $privpath/testnet.flag)

mkdir -p $assets

# Wait time (in seconds) before querying blockchain
if [ $mN == $sanchomagic ]; then
    pause=7
else
    pause=4
fi

# parkingTag=43
unitRedeemer=$assets/unit.cbor
stateB=$assets/datumB.cbor
rollupLovelaceValue=3000000
nftPolicy=$assets/nftPolicy.plutus
dataPolicy=$assets/rollupData.plutus
dataRedeemer=$assets/dataRedeemer.cbor

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file  /dev/stdout | jq -r 'keys[0]')
in1Address=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file  /dev/stdout | jq -r 'to_entries[0].value.address')

echo ""
echo "Initialization..."
echo ""

#-------------------------------- :protocol parameters: ------------------------------

cardano-cli conway query protocol-parameters \
  --testnet-magic $mN \
  --out-file $assets/protocol.json

#------------------------------- :create scripts: ------------------------------

cabal run rollup-init-transaction -- $in1 $in1Address

#-------------------------------- :rollup setup: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/rollup.plutus" \
    --out-file "$keypath/rollup.addr" \
    --testnet-magic $mN

#-------------------------------- :parkingSpot setup: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/parkingSpot.plutus" \
    --out-file "$keypath/parkingSpot.addr" \
    --testnet-magic $mN

#-------------------------------- :park rollup script: -------------------------------

echo "Parking 'rollup.plutus'..."

in2=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file  /dev/stdout | jq -r 'keys[1]')

parkScriptMinCost=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $assets/protocol.json \
  --tx-out $(cat $keypath/parkingSpot.addr)+0 \
  --tx-out-reference-script-file "$assets/rollup.plutus" | sed 's/^[^ ]* //')

cardano-cli conway transaction build \
  --testnet-magic $mN \
  --tx-in $in2 \
  --tx-in-collateral $in2 \
  --tx-out "$(cat $keypath/parkingSpot.addr) + $parkScriptMinCost lovelace" \
  --tx-out-reference-script-file $assets/rollup.plutus \
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

parkedTx=$(cardano-cli conway transaction txid --tx-file "$keypath/parkedScript.tx")
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

#------------------------------ :send update data token: -----------------------------

echo "Sending update data token..."

in3=$parkedTx#1
dataPolicyId=$(cardano-cli conway transaction policyid --script-file $dataPolicy)
dataTokenName=$(cat $assets/dataTokenName.txt)

cardano-cli conway transaction build \
  --testnet-magic $mN \
  --tx-in $in3 \
  --tx-in-collateral $in3 \
  --tx-out "$(cat $keypath/parkingSpot.addr) + $rollupLovelaceValue lovelace + 1 $dataPolicyId.$dataTokenName" \
  --change-address $(cat $keypath/alice.addr) \
  --mint "1 $dataPolicyId.$dataTokenName" \
  --mint-script-file $dataPolicy \
  --mint-redeemer-cbor-file $dataRedeemer \
  --out-file $keypath/dataRef.txbody

cardano-cli conway transaction sign \
  --testnet-magic $mN \
  --tx-body-file $keypath/dataRef.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/dataRef.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/dataRef.tx

cp $keypath/dataRef.tx $keypath/prevRollupOutA.tx

dataRefTx=$(cardano-cli conway transaction txid --tx-file "$keypath/dataRef.tx")
dataRefOut=$dataRefTx#0
while true; do
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/parkingSpot.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$dataRefOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see update data token onchain..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $dataRefTx"
	echo ""
	break
    fi
done

#-------------------------------- :NFT mint: -------------------------------

echo "Transfering initial state..."

nftPolicyId=$(cardano-cli conway transaction policyid --script-file $nftPolicy)

cardano-cli conway transaction build \
  --testnet-magic $mN \
  --tx-in $in1 \
  --tx-in-collateral $in1 \
  --tx-out "$(cat $keypath/rollup.addr) + $rollupLovelaceValue lovelace + 1 $nftPolicyId.7a6b466f6c64" \
  --tx-out-inline-datum-cbor-file $stateB \
  --change-address $(cat $keypath/alice.addr) \
  --mint "1 $nftPolicyId.7a6b466f6c64" \
  --mint-script-file $nftPolicy \
  --mint-redeemer-cbor-file $unitRedeemer \
  --out-file $keypath/rollupOutB.txbody

cardano-cli conway transaction sign \
  --testnet-magic $mN \
  --tx-body-file $keypath/rollupOutB.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/rollupOutB.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/rollupOutB.tx

rollupTx=$(cardano-cli conway transaction txid --tx-file "$keypath/rollupOutB.tx")
rollupOut=$rollupTx#0
while true; do
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/rollup.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$rollupOut" 'has($key) | tostring')
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
