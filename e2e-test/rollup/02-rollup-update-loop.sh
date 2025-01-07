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

counter=0
protocolParams=$assets/protocol.json

unitDatum=$assets/unit.cbor
unitRedeemer=$assets/unit.cbor

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

parkingSpotPolicy=$assets/parkingSpot.plutus

collateral=$(cardano-cli transaction txid --tx-file "$keypath/splitAlice.tx")#0

#---------------------------- :numeric parameters: ----------------------------

rollupLovelaceValue=3000000  # Value (in lovelaces) to be transfered with each rollup
rollupFee=15000000  # rollup fee

#-------------------------------- :rollup loop: -------------------------------

loop=true
printf "$loop" > $keypath/rollup-loop.flag

while $loop; do
    inR=$(cardano-cli transaction txid --tx-file "$keypath/rollupOut.tx")#0
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/rollup.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$inR" 'has($key) | tostring')

    if [ $txOnChain == "false" ]; then
	echo "Waiting to see rollup tx onchain..."
	sleep $pause
    else
	echo ""
	echo "Starting next rollup update..."
	echo ""

	cabal run rollup-update-loop

	#------------------------------ :send update data token: -----------------------------

	echo "Sending update data token..."
	echo ""

	dataUtxoTx=$(cardano-cli conway transaction txid --tx-file "$keypath/prevDataRef.tx")
	dataUtxo=$dataUtxoTx#0
	if [ $counter -le 1 ]; then
	    echo "using input 1"
	    # echo $(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN)
	    in2=$dataUtxoTx#1
	else
	    echo "using input 2"
	    # echo $(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN)
	    in2=$dataUtxoTx#2
	fi

	dataPolicyId=$(cardano-cli conway transaction policyid --script-file $dataPolicy)
	dataTokenName=$(cat $assets/dataNewTokenName.txt)
        dataTokens=$(cat $assets/dataTokens.txt)
	dataTokensDiscardedFile="$assets/dataTokensDiscarded.txt"

        tokensAmount=$(cat $assets/dataTokensAmount.txt)
	
	echo "Reference UTxO with $tokensAmount data tokens:"
	echo "$dataTokens"
        echo ""

	dataTokensMinCost=$(cardano-cli conway transaction calculate-min-required-utxo \
	  --protocol-params-file $assets/protocol.json \
	  --tx-out "$(cat $keypath/parkingSpot.addr) + $dataTokens" \
	  --tx-out-inline-datum-cbor-file $unitDatum | sed 's/^[^ ]* //')

	if [ ! -s $dataTokensDiscardedFile ]; then
	    echo "branch 1"
	    cardano-cli conway transaction build \
	      --testnet-magic $mN \
	      --tx-in $in2 \
	      --tx-in $dataUtxo \
	      --tx-in-script-file $parkingSpotPolicy \
	      --tx-in-inline-datum-present \
	      --tx-in-redeemer-cbor-file $unitRedeemer \
	      --tx-in-collateral $collateral \
	      --tx-out "$(cat $keypath/parkingSpot.addr) + $dataTokensMinCost lovelace + $dataTokens" \
	      --tx-out-inline-datum-cbor-file $unitDatum \
	      --change-address $(cat $keypath/alice.addr) \
	      --mint "1 $dataPolicyId.$dataTokenName" \
	      --mint-script-file $dataPolicy \
	      --mint-redeemer-cbor-file $dataRedeemer \
	      --out-file $keypath/dataRef.txbody
	else
	    echo "branch 2"
	    dataTokensDiscarded=$(cat $dataTokensDiscardedFile)
	    dataTokensDiscardedMinCost=$(cardano-cli conway transaction calculate-min-required-utxo \
	      --protocol-params-file $assets/protocol.json \
	      --tx-out "$(cat $keypath/parkingSpot.addr) + $dataTokensDiscarded" \
	      --tx-out-inline-datum-cbor-file $unitDatum | sed 's/^[^ ]* //')

	    cardano-cli conway transaction build \
	      --testnet-magic $mN \
	      --tx-in $in2 \
	      --tx-in $dataUtxo \
	      --tx-in-script-file $parkingSpotPolicy \
	      --tx-in-inline-datum-present \
	      --tx-in-redeemer-cbor-file $unitRedeemer \
	      --tx-in-collateral $collateral \
	      --tx-out "$(cat $keypath/parkingSpot.addr) + $dataTokensMinCost lovelace + $dataTokens" \
	      --tx-out "$(cat $keypath/parkingSpot.addr) + $dataTokensDiscardedMinCost lovelace + $dataTokensDiscarded" \
	      --tx-out-inline-datum-cbor-file $unitDatum \
	      --change-address $(cat $keypath/alice.addr) \
	      --mint "1 $dataPolicyId.$dataTokenName" \
	      --mint-script-file $dataPolicy \
	      --mint-redeemer-cbor-file $dataRedeemer \
	      --out-file $keypath/dataRef.txbody
	fi

	# cardano-cli conway transaction build \
	#   --testnet-magic $mN \
	#   --tx-in $in2 \
	#   --tx-in $dataUtxo \
	#   --tx-in-script-file $parkingSpotPolicy \
	#   --tx-in-inline-datum-present \
	#   --tx-in-redeemer-cbor-file $unitRedeemer \
	#   --tx-in-collateral $collateral \
	#   --tx-out "$(cat $keypath/parkingSpot.addr) + $dataTokensMinCost lovelace + $dataTokens" \
	#   --tx-out-inline-datum-cbor-file $unitDatum \
	#   --change-address $(cat $keypath/alice.addr) \
	#   --mint "1 $dataPolicyId.$dataTokenName" \
	#   --mint-script-file $dataPolicy \
	#   --mint-redeemer-cbor-file $dataRedeemer \
	#   --out-file $keypath/dataRef.txbody

	cardano-cli conway transaction sign \
	  --testnet-magic $mN \
	  --tx-body-file $keypath/dataRef.txbody \
	  --signing-key-file $keypath/alice.skey \
	  --out-file $keypath/dataRef.tx

	cardano-cli conway transaction submit \
	    --testnet-magic $mN \
	    --tx-file $keypath/dataRef.tx

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

	cp $keypath/dataRef.tx $keypath/prevDataRef.tx

	#-------------------------------- :rollup transaction: -------------------------------

	echo "Rollup transaction..."
	echo ""

	aliceIdx=$(cat $privpath/aliceIdx.flag)
	in1=$(cardano-cli conway transaction txid --tx-file "$keypath/rollupOut.tx")#$aliceIdx

	cardano-cli conway transaction build \
	  --testnet-magic $mN \
	  --tx-in $in1 \
	  --tx-in $inR \
	  --spending-tx-in-reference $rollupScript \
	  --spending-plutus-script-v3 \
	  --spending-reference-tx-in-inline-datum-present \
	  --spending-reference-tx-in-redeemer-cbor-file $rollupRedeemer \
	  --read-only-tx-in-reference $(cardano-cli conway transaction txid --tx-file "$keypath/dataRef.tx")#0 \
	  --tx-in-collateral $collateral \
	  --tx-out "$(cat $keypath/rollup.addr) + $rollupLovelaceValue lovelace + 1 $nftPolicyId.$nftPolicyNm" \
	  --tx-out-inline-datum-cbor-file $state \
	  --tx-out "$(cat $keypath/bob.addr) + $rollupFee lovelace" \
	  --change-address $(cat $keypath/alice.addr) \
	  --out-file $keypath/rollupOut.txbody

	cardano-cli conway transaction sign \
	  --testnet-magic $mN \
	  --tx-body-file $keypath/rollupOut.txbody \
	  --signing-key-file $keypath/alice.skey \
	  --out-file $keypath/nextRollupOut.tx

	cardano-cli conway transaction submit \
	    --testnet-magic $mN \
	    --tx-file $keypath/nextRollupOut.tx

	#-------------------------------- :rollup summary: -------------------------------

	counter=$((counter + 1))

	echo ""
	echo "Rollup-update completed.  Rollups completed: $counter."
	echo "Transaction Id: $(cardano-cli transaction txid --tx-file $keypath/nextRollupOut.tx)"

    #-------------------------------- :cleanup before next batch: -------------------------------

    printf "2" > $privpath/aliceIdx.flag
	
    mv $keypath/nextRollupOut.tx $keypath/rollupOut.tx
    mv $assets/newRollupInfo.json $assets/rollupInfo.json
    mv $assets/newDataTokens.txt $assets/dataTokens.txt
    mv $assets/newDataTokensDiscarded.txt $assets/dataTokensDiscarded.txt
    mv $assets/newDataTokensAmount.txt $assets/dataTokensAmount.txt

    fi
    loop=$(cat $keypath/rollup-loop.flag)
done
