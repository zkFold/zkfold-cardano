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
dataRedeemer03=$assets/dataRedeemer-03.cbor

bridgeHash=$assets/bridgeDatumHash.txt

nftPolicy=$assets/nftPolicy.plutus
nftPolicyId=$(cardano-cli conway transaction policyid --script-file $nftPolicy)
nftPolicyNm="7a6b466f6c64"  # token name: "zkFold"

parkingSpotAddr=$(cat $keypath/parkingSpot.addr)

collateral=$(cardano-cli transaction txid --tx-file "$keypath/splitAlice.tx")#0

#---------------------------- :numeric parameters: ----------------------------

rollupLovelaceValue=3000000  # Value (in lovelaces) to be transfered with each rollup
rollupFee=15000000  # rollup fee
rollupsBeforeCleanup=5  # number of rollups before data token cleanup

#---------------------------------- :macros: ----------------------------------

query_utxo_has_key() {
    local addr=$1
    local txout=$2
    cardano-cli query utxo --address $addr --testnet-magic $mN --out-file /dev/stdout |
        jq -r --arg key $txout 'has($key) | tostring'
}

data_token_tx_build () {	
    local in=$1
    local dataTokenName=$2
    local dataRedeemer=$3
    local txbodyFile=$4
    cardano-cli conway transaction build \
      --testnet-magic $mN \
      --tx-in $in \
      --tx-in-collateral $collateral \
      --tx-out "$(cat $keypath/parkingSpot.addr) + $dataTokensMinCost lovelace + 1 $dataPolicyId.$dataTokenName" \
      --tx-out-inline-datum-cbor-file $unitDatum \
      --change-address $(cat $keypath/alice.addr) \
      --mint "1 $dataPolicyId.$dataTokenName" \
      --mint-script-file $dataPolicy \
      --mint-redeemer-cbor-file $dataRedeemer \
      --out-file $keypath/$txbodyFile
}

data_token_tx_sign () {
    local txbodyFile=$1
    local txFile=$2
    cardano-cli conway transaction sign \
      --testnet-magic $mN \
      --tx-body-file $keypath/$txbodyFile \
      --signing-key-file $keypath/alice.skey \
      --out-file $keypath/$txFile
}
    
data_token_tx_submit () {
    local txFile=$1
    cardano-cli conway transaction submit \
	--testnet-magic $mN \
	--tx-file $keypath/$txFile
}

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
	if [ $((rollupCounter % rollupsBeforeCleanup)) -eq 0 ] && [ $rollupCounter -gt 0 ]; then
	    ./rollup/data-cleanup.sh  # Regularly burn used data tokens
	fi

	echo ""
	echo "Starting next rollup update..."
	echo ""

	cabal run rollup-update-loop

	#------------------------------ :send update data token: -----------------------------

	echo "Sending update data tokens..."
	echo ""

	if [ ! -f $keypath/prevDataRef-01.tx ]; then
	    in201=$(cardano-cli conway transaction txid --tx-file "$keypath/parkedScript.tx")#1
	    in202=$(cardano-cli conway transaction txid --tx-file "$keypath/parkedScript.tx")#2
	    in203=$(cardano-cli conway transaction txid --tx-file "$keypath/parkedScript.tx")#3
	else
	    in201=$(cardano-cli conway transaction txid --tx-file "$keypath/prevDataRef-01.tx")#1
	    in202=$(cardano-cli conway transaction txid --tx-file "$keypath/prevDataRef-02.tx")#1
	    in203=$(cardano-cli conway transaction txid --tx-file "$keypath/prevDataRef-03.tx")#1
	fi	    

	dataPolicyId=$(cardano-cli conway transaction policyid --script-file $dataPolicy)
	dataTokenName01=$(cat $assets/dataTokenName-01.txt)
	dataTokenName02=$(cat $assets/dataTokenName-02.txt)
	dataTokenName03=$(cat $assets/dataTokenName-03.txt)

        tokensAmount=$(cat $assets/dataTokensAmount.txt)
	
	echo "Will use $tokensAmount reference UTxOs with data tokens."
        echo ""

	dataTokensMinCost=$(cardano-cli conway transaction calculate-min-required-utxo \
	  --protocol-params-file $assets/protocol.json \
	  --tx-out "$(cat $keypath/parkingSpot.addr) + 1 $dataPolicyId.$dataTokenName01" \
	  --tx-out-inline-datum-cbor-file $unitDatum | sed 's/^[^ ]* //')

	data_token_tx_build $in201 $dataTokenName01 $dataRedeemer01 "dataRef-01.txbody"
	data_token_tx_build $in202 $dataTokenName02 $dataRedeemer02 "dataRef-02.txbody"
	data_token_tx_build $in203 $dataTokenName03 $dataRedeemer03 "dataRef-03.txbody"

	data_token_tx_sign "dataRef-01.txbody" "dataRef-01.tx"
	data_token_tx_sign "dataRef-02.txbody" "dataRef-02.tx"
	data_token_tx_sign "dataRef-03.txbody" "dataRef-03.tx"

	data_token_tx_submit "dataRef-01.tx"
	data_token_tx_submit "dataRef-02.tx"
	data_token_tx_submit "dataRef-03.tx"

	dataRefTx01=$(cardano-cli conway transaction txid --tx-file "$keypath/dataRef-01.tx")
	dataRefTx02=$(cardano-cli conway transaction txid --tx-file "$keypath/dataRef-02.tx")
	dataRefTx03=$(cardano-cli conway transaction txid --tx-file "$keypath/dataRef-03.tx")
	dataRefOut01=$dataRefTx01#0
	dataRefOut02=$dataRefTx02#0
	dataRefOut03=$dataRefTx03#0
	while true; do
	    txOnChain01=$(query_utxo_has_key $parkingSpotAddr $dataRefOut01)
	    txOnChain02=$(query_utxo_has_key $parkingSpotAddr $dataRefOut02)
	    txOnChain03=$(query_utxo_has_key $parkingSpotAddr $dataRefOut03)
	    if [ $txOnChain01 == "false" ] || [ $txOnChain02 == "false" ] || [ $txOnChain03 == "false" ]; then
		echo "Waiting to see update data tokens onchain..."
		sleep $pause
	    else
		echo ""
		echo "Transaction Id: $dataRefTx01"
		echo "Transaction Id: $dataRefTx02"
		echo "Transaction Id: $dataRefTx03"
		echo ""
		break
	    fi
	done

	cp $keypath/dataRef-01.tx $keypath/prevDataRef-01.tx
	cp $keypath/dataRef-02.tx $keypath/prevDataRef-02.tx
	cp $keypath/dataRef-03.tx $keypath/prevDataRef-03.tx

	#-------------------------------- :rollup transaction: -------------------------------

	echo "Rollup transaction..."
	echo ""

	aliceIdx=$(cat $privpath/aliceIdx.flag)
	dataCleaned=$(cat $privpath/dataCleaned.flag)
	if [ "$dataCleaned" -eq 0 ]; then
	    in1=$(cardano-cli conway transaction txid --tx-file "$keypath/rollupOut.tx")#$aliceIdx
	else
	    in1=$(cardano-cli conway transaction txid --tx-file "$keypath/dataClean.tx")#0
	fi

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
	  --read-only-tx-in-reference $(cardano-cli conway transaction txid --tx-file "$keypath/dataRef-03.tx")#0 \
	  --tx-in-collateral $collateral \
	  --tx-out "$(cat $keypath/rollup.addr) + $rollupLovelaceValue lovelace + 1 $nftPolicyId.$nftPolicyNm" \
	  --tx-out-inline-datum-cbor-file $state \
	  --tx-out "$(cat $keypath/bob.addr) + $rollupFee lovelace" \
	  --tx-out "$(cat $keypath/parkingSpot.addr) + 995610 lovelace" \
	  --tx-out-datum-hash $(cat $bridgeHash) \
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

    printf "3" > $privpath/aliceIdx.flag
    printf "0" > $privpath/dataCleaned.flag
    printf "$rollupCounter" > $privpath/rollupCounter.var
	
    mv $keypath/nextRollupOut.tx $keypath/rollupOut.tx
    mv $assets/newRollupInfo.json $assets/rollupInfo.json
    mv $assets/newDataTokensAmount.txt $assets/dataTokensAmount.txt

    fi
    loop=$(cat $keypath/rollup-loop.flag)
done
