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

unitDatum=$assets/unit.cbor
unitRedeemer=$assets/unit.cbor

dataPolicy=$assets/rollupData.plutus
dataCleanRedeemer=$assets/dataCleanRedeemer.cbor

parkingSpotPolicy=$assets/parkingSpot.plutus

collateral=$(cardano-cli transaction txid --tx-file "$keypath/splitAlice.tx")#0

#---------------------------- :data tokens cleanup: ---------------------------

dataPolicyId=$(cardano-cli conway transaction policyid --script-file $dataPolicy)

cardano-cli query utxo --address $(cat $keypath/parkingSpot.addr) --testnet-magic $mN --out-file /dev/stdout |
           jq -r --arg key $dataPolicyId 'to_entries | map(select(.value.value | has($key)))' > $assets/dataTokensBurn.json

cabal run rollup-clear-data
chmod +x $assets/burnDataTokens.sh

$assets/burnDataTokens.sh

cardano-cli conway transaction sign \
  --testnet-magic $mN \
  --tx-body-file $keypath/dataClean.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/dataClean.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/dataClean.tx

dataCleanTx=$(cardano-cli conway transaction txid --tx-file "$keypath/dataClean.tx")
dataCleanOut=$dataCleanTx#0
while true; do
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$dataCleanOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see data token's burning transaction onchain..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $dataCleanTx"
	break
    fi
done

printf "1" > $privpath/dataCleaned.flag
