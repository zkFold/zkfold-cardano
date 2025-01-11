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

rollupCounter=$(cat $privpath/rollupCounter.var)
protocolParams=$assets/protocol.json

unitDatum=$assets/unit.cbor
unitRedeemer=$assets/unit.cbor

state=$assets/datum.cbor

rollupRedeemer=$assets/redeemerRollup.cbor
rollupScript=$(cardano-cli transaction txid --tx-file "$keypath/parkedScript.tx")#0
rollupScriptFile=$assets/rollup.plutus
rollupVerifierSize=$(stat -c%s "$rollupScriptFile")
rollupAddr=$(cat $keypath/rollup.addr)

dataPolicy=$assets/rollupData.plutus
dataRedeemer01=$assets/dataRedeemer-01.cbor
dataRedeemer02=$assets/dataRedeemer-02.cbor

nftPolicy=$assets/nftPolicy.plutus
nftPolicyId=$(cardano-cli conway transaction policyid --script-file $nftPolicy)
nftPolicyNm="7a6b466f6c64"  # token name: "zkFold"

parkingSpotPolicy=$assets/parkingSpot.plutus
parkingSpotAddr=$(cat $keypath/parkingSpot.addr)

collateral=$(cardano-cli transaction txid --tx-file "$keypath/splitAlice.tx")#0

#---------------------------------- :macros: ----------------------------------

query_utxo_has_key() {
    local addr=$1
    local txout=$2
    cardano-cli query utxo --address $addr --testnet-magic $mN --out-file /dev/stdout |
        jq -r --arg key $txout 'has($key) | tostring'
}

#---------------------------- :numeric parameters: ----------------------------

rollupLovelaceValue=3000000  # Value (in lovelaces) to be transfered with each rollup
rollupFee=15000000  # rollup fee

#-------------------------------- :rollup loop: -------------------------------

loop=true
printf "$loop" > $keypath/rollup-loop.flag

while $loop; do
    inR=$(cardano-cli transaction txid --tx-file "$keypath/rollupOut.tx")#0
    txOnChain=$(query_utxo_has_key $rollupAddr $inR)

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

	if [ ! -f $keypath/prevDataRef-01.tx ]; then
	    in201=$(cardano-cli conway transaction txid --tx-file "$keypath/parkedScript.tx")#1
	    in202=$(cardano-cli conway transaction txid --tx-file "$keypath/parkedScript.tx")#2
	else
	    in201=$(cardano-cli conway transaction txid --tx-file "$keypath/prevDataRef-01.tx")#1
	    in202=$(cardano-cli conway transaction txid --tx-file "$keypath/prevDataRef-02.tx")#1
	fi	    

	dataPolicyId=$(cardano-cli conway transaction policyid --script-file $dataPolicy)
	dataTokenName01=$(cat $assets/dataTokenName-01.txt)
	dataTokenName02=$(cat $assets/dataTokenName-02.txt)

        tokensAmount=$(cat $assets/dataTokensAmount.txt)
	
	echo "Will use $tokensAmount reference UTxOs with data tokens."
        echo ""

	dataTokensMinCost=$(cardano-cli conway transaction calculate-min-required-utxo \
	  --protocol-params-file $assets/protocol.json \
	  --tx-out "$(cat $keypath/parkingSpot.addr) + 1 $dataPolicyId.$dataTokenName01" \
	  --tx-out-inline-datum-cbor-file $unitDatum | sed 's/^[^ ]* //')

	cardano-cli conway transaction build \
	  --testnet-magic $mN \
	  --tx-in $in201 \
	  --tx-in-collateral $collateral \
	  --tx-out "$(cat $keypath/parkingSpot.addr) + $dataTokensMinCost lovelace + 1 $dataPolicyId.$dataTokenName01" \
	  --tx-out-inline-datum-cbor-file $unitDatum \
	  --change-address $(cat $keypath/alice.addr) \
	  --mint "1 $dataPolicyId.$dataTokenName01" \
	  --mint-script-file $dataPolicy \
	  --mint-redeemer-cbor-file $dataRedeemer01 \
	  --out-file $keypath/dataRef-01.txbody

	cardano-cli conway transaction build \
	  --testnet-magic $mN \
	  --tx-in $in202 \
	  --tx-in-collateral $collateral \
	  --tx-out "$(cat $keypath/parkingSpot.addr) + $dataTokensMinCost lovelace + 1 $dataPolicyId.$dataTokenName02" \
	  --tx-out-inline-datum-cbor-file $unitDatum \
	  --change-address $(cat $keypath/alice.addr) \
	  --mint "1 $dataPolicyId.$dataTokenName02" \
	  --mint-script-file $dataPolicy \
	  --mint-redeemer-cbor-file $dataRedeemer02 \
	  --out-file $keypath/dataRef-02.txbody

	cardano-cli conway transaction sign \
	  --testnet-magic $mN \
	  --tx-body-file $keypath/dataRef-01.txbody \
	  --signing-key-file $keypath/alice.skey \
	  --out-file $keypath/dataRef-01.tx

	cardano-cli conway transaction sign \
	  --testnet-magic $mN \
	  --tx-body-file $keypath/dataRef-02.txbody \
	  --signing-key-file $keypath/alice.skey \
	  --out-file $keypath/dataRef-02.tx

	cardano-cli conway transaction submit \
	    --testnet-magic $mN \
	    --tx-file $keypath/dataRef-01.tx

	cardano-cli conway transaction submit \
	    --testnet-magic $mN \
            --tx-file $keypath/dataRef-02.tx

	dataRefTx01=$(cardano-cli conway transaction txid --tx-file "$keypath/dataRef-01.tx")
	dataRefTx02=$(cardano-cli conway transaction txid --tx-file "$keypath/dataRef-02.tx")
	dataRefOut01=$dataRefTx01#0
	dataRefOut02=$dataRefTx02#0
	while true; do
	    txOnChain01=$(query_utxo_has_key $parkingSpotAddr $dataRefOut01)
	    txOnChain02=$(query_utxo_has_key $parkingSpotAddr $dataRefOut02)	    
	    if [ $txOnChain01 == "false" ] || [ $txOnChain02 == "false" ]; then
		echo "Waiting to see update data tokens onchain..."
		sleep $pause
	    else
		echo ""
		echo "Transaction Id: $dataRefTx01"
		echo "Transaction Id: $dataRefTx02"
		echo ""
		break
	    fi
	done

	cp $keypath/dataRef-01.tx $keypath/prevDataRef-01.tx
	cp $keypath/dataRef-02.tx $keypath/prevDataRef-02.tx

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
	  --read-only-tx-in-reference $(cardano-cli conway transaction txid --tx-file "$keypath/dataRef-01.tx")#0 \
	  --read-only-tx-in-reference $(cardano-cli conway transaction txid --tx-file "$keypath/dataRef-02.tx")#0 \
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

	rollupCounter=$((rollupCounter + 1))

	echo ""
	echo "Rollup-update completed.  Rollups completed: $rollupCounter."
	echo "Transaction Id: $(cardano-cli transaction txid --tx-file $keypath/nextRollupOut.tx)"

    #-------------------------------- :cleanup before next batch: -------------------------------

    printf "2" > $privpath/aliceIdx.flag
    printf "$rollupCounter" > $privpath/rollupCounter.var
	
    mv $keypath/nextRollupOut.tx $keypath/rollupOut.tx
    mv $assets/newRollupInfo.json $assets/rollupInfo.json
    mv $assets/newDataTokensAmount.txt $assets/dataTokensAmount.txt

    fi
    loop=$(cat $keypath/rollup-loop.flag)
done
