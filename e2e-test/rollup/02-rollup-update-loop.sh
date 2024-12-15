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

# Wait time (in seconds) before querying blockchain
if [ $mN == $sanchomagic ]; then
    pause=7
else
    pause=4
fi

protocolParams=$assets/protocol.json

export aliceIdx=1
state=$assets/datum.cbor
rollupRedeemer=$assets/redeemerRollup.cbor

rollupScript=$(cardano-cli transaction txid --tx-file "$keypath/parkedScript.tx")#0
rollupScriptFile=$assets/rollup.plutus
rollupVerifierSize=$(stat -c%s "$rollupScriptFile")

dataPolicy=$assets/rollupData.plutus
dataRedeemer=$assets/dataRedeemer.cbor
nftPolicy=$assets/nftPolicy.plutus
nftPolicyId=$(cardano-cli conway transaction policyid --script-file $nftPolicy)
nftPolicyNm="7a6b466f6c64"  # token name: "zkFold"

#-------------------------------- :numeric parameters: -------------------------------

rollupLovelaceValue=3000000  # Value (in lovelaces) to be transfered with each rollup
rollupFee=15000000  # rollup fee

#-------------------------------- :rollup loop: -------------------------------

loop=true
printf "$loop" > $keypath/rollup-loop.flag

while $loop; do
    dataLength=$(cat $assets/dataUpdateLength.txt)
    inR=$(cardano-cli transaction txid --tx-file "$keypath/rollupOut.tx")#0
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/rollup.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$inR" 'has($key) | tostring')

    if [ $txOnChain == "false" ]; then
	echo "Waiting to see rollup tx onchain..."
	sleep $pause
    else
	echo ""
	echo "Starting next rollup update..."
	echo ""

	cabal run rollup-update-loop -- $dataLength
	chmod +x $assets/rollupCLICode.sh

	#------------------------------ :send update data token: -----------------------------

	echo "Sending update data token..."
	echo ""

	in2=$(cardano-cli conway transaction txid --tx-file "$keypath/prevDataRef.tx")#1
	dataPolicyId=$(cardano-cli conway transaction policyid --script-file $dataPolicy)
	dataTokenName=$(cat $assets/dataTokenName.txt)

	cardano-cli conway transaction build \
	  --testnet-magic $mN \
	  --tx-in $in2 \
	  --tx-in-collateral $in2 \
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
	  --out-file $keypath/dataRef-$dataLength.tx

	cardano-cli conway transaction submit \
	    --testnet-magic $mN \
	    --tx-file $keypath/dataRef-$dataLength.tx

	dataRefTx=$(cardano-cli conway transaction txid --tx-file "$keypath/dataRef-$dataLength.tx")
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

	cp $keypath/dataRef-$dataLength.tx $keypath/prevDataRef.tx

	#-------------------------------- :rollup transaction: -------------------------------

	echo "Rollup transaction..."
	echo ""

	# in1=$(cardano-cli conway transaction txid --tx-file "$keypath/rollupOut.tx")#$aliceIdx

	# # Build first Tx
	# cardano-cli conway transaction build \
	#   --testnet-magic $mN \
	#   --tx-in $in1 \
	#   --tx-in $inR \
	#   --spending-tx-in-reference $rollupScript \
	#   --spending-plutus-script-v3 \
	#   --spending-reference-tx-in-inline-datum-present \
	#   --spending-reference-tx-in-redeemer-cbor-file $rollupRedeemer \
	#   --read-only-tx-in-reference $(cardano-cli conway transaction txid --tx-file "$keypath/dataRef-0.tx")#0 \
	#   --tx-in-collateral $in1 \
	#   --tx-out "$(cat $keypath/rollup.addr) + $rollupLovelaceValue lovelace + 1 $nftPolicyId.$nftPolicyNm" \
	#   --tx-out-inline-datum-cbor-file $state \
	#   --tx-out "$(cat $keypath/alice.addr) + $rollupFee lovelace" \
	#   --change-address $(cat $keypath/alice.addr) \
	#   --out-file $keypath/rollupOut.txbody

	$assets/rollupCLICode.sh

	cardano-cli conway transaction sign \
	  --testnet-magic $mN \
	  --tx-body-file $keypath/rollupOut.txbody \
	  --signing-key-file $keypath/alice.skey \
	  --out-file $keypath/nextRollupOut.tx

	cardano-cli conway transaction submit \
	    --testnet-magic $mN \
	    --tx-file $keypath/nextRollupOut.tx

	#-------------------------------- :rollup summary: -------------------------------

	echo ""
	echo "Rollup-update completed.  Length of data update: $(cat $assets/dataUpdateLength.txt)."
	echo "Transaction Id: $(cardano-cli transaction txid --tx-file $keypath/nextRollupOut.tx)"

	#-------------------------------- :cleanup before next batch: -------------------------------

    export aliceIdx=2
	
    mv $keypath/nextRollupOut.tx $keypath/rollupOut.tx
    mv $assets/newRollupInfo.json $assets/rollupInfo.json

    fi
    loop=$(cat $keypath/rollup-loop.flag)
done
