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

unitRedeemer=$assets/unit.cbor
state=$assets/datum.cbor
rollupLovelaceValue=3000000
nftPolicy=$assets/nftPolicy.plutus

in1=$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file /dev/stdout |
	  jq -r 'to_entries | map(select(.value.value.lovelace > 5000000)) | .[0].key')
in2=$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file /dev/stdout |
	  jq -r 'to_entries | map(select(.value.value.lovelace > 5000000)) | .[1].key')

echo ""
echo "Initialization..."
echo ""

#-------------------------------- :protocol parameters: ------------------------------

cardano-cli conway query protocol-parameters \
  --testnet-magic $mN \
  --out-file $assets/protocol.json

#---------------------------------- :initialize Bob: ---------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/bob.vkey \
  --signing-key-file $keypath/bob.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/bob.vkey \
  --out-file $keypath/bob.addr \
  --testnet-magic $mN

bobAddress=$(cat $keypath/bob.addr)

#------------------------------- :create scripts: ------------------------------

cabal run rollup-init-transaction -- $in1 $bobAddress

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

#----------------------------- :NFT mint & initial state: ----------------------------

echo "Transfering initial state..."

nftPolicyId=$(cardano-cli conway transaction policyid --script-file $nftPolicy)
nftPolicyNm="7a6b466f6c64"  # token name: "zkFold"

cardano-cli conway transaction build \
  --testnet-magic $mN \
  --tx-in $in1 \
  --tx-in-collateral $in1 \
  --tx-out "$(cat $keypath/rollup.addr) + $rollupLovelaceValue lovelace + 1 $nftPolicyId.$nftPolicyNm" \
  --tx-out-inline-datum-cbor-file $state \
  --change-address $(cat $keypath/alice.addr) \
  --mint "1 $nftPolicyId.$nftPolicyNm" \
  --mint-script-file $nftPolicy \
  --mint-redeemer-cbor-file $unitRedeemer \
  --out-file $keypath/rollupOut.txbody

cardano-cli conway transaction sign \
  --testnet-magic $mN \
  --tx-body-file $keypath/rollupOut.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/rollupOut.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/rollupOut.tx

rollupTx=$(cardano-cli conway transaction txid --tx-file "$keypath/rollupOut.tx")
rollupOut=$rollupTx#0
while true; do
    txOnChain=$(cardano-cli conway query utxo --address $(cat $keypath/rollup.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$rollupOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see initial rollup tx onchain..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $rollupTx"
	echo ""
	break
    fi
done

#-------------------------------- :park rollup script: -------------------------------

echo "Parking 'rollup.plutus'..."

parkScriptMinCost=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $assets/protocol.json \
  --tx-out $(cat $keypath/parkingSpot.addr)+0 \
  --tx-out-reference-script-file "$assets/rollup.plutus" | sed 's/^[^ ]* //')

in2value=$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$in2" '.[$key].value.lovelace')

cardano-cli conway transaction build \
  --testnet-magic $mN \
  --tx-in $in2 \
  --tx-out "$(cat $keypath/parkingSpot.addr) + $parkScriptMinCost lovelace" \
  --tx-out-reference-script-file $assets/rollup.plutus \
  --tx-out "$(cat $keypath/alice.addr) + $((in2value / 3)) lovelace" \
  --tx-out "$(cat $keypath/alice.addr) + $((in2value / 3)) lovelace" \
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
    txOnChain=$(cardano-cli conway query utxo --address $(cat $keypath/parkingSpot.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$parkedOut" 'has($key) | tostring')
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

#-------------------------------- :epilogue: -------------------------------

printf "0" > $privpath/rollupCounter.var
printf "1" > $privpath/aliceIdx.flag
printf "0" > $privpath/dataCleaned.flag

echo ""
echo "Initialization completed."
echo ""
