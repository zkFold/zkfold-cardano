#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

sanchomagic=4
assets=../assets
keypath=./plonkupVerifierTx-balancing/keys
privpath=./plonkupVerifierTx-balancing/priv

mN=$(cat $privpath/testnet.flag)

mkdir -p $assets

# Wait time (in seconds) before querying blockchain
if [ $mN == $sanchomagic ]; then
    pause=7
else
    pause=4
fi

unitDatum=$assets/unit.cbor
someDatum=$assets/someDatum.cbor

echo ""
echo "Initialization..."
echo ""

#-------------------------------- :protocol parameters: ------------------------------

cardano-cli conway query protocol-parameters \
  --testnet-magic $mN \
  --out-file $assets/protocol.json

#------------------------------- :create scripts: ------------------------------

cabal run balancing-init-transaction

#--------------------------- :plonkupVerifierTx setup: ---------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/plonkupVerifierTx.plutus" \
    --out-file "$keypath/plonkupVerifierTx.addr" \
    --testnet-magic $mN

#----------------------------- :parkingSpot setup: -----------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/parkingSpot.plutus" \
    --out-file "$keypath/parkingSpot.addr" \
    --testnet-magic $mN

#------------------------ :park plonkupVerifierTx script: ------------------------

echo "Parking 'plonkupVerifierTx.plutus'..."

in1=$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file  /dev/stdout | jq -r 'to_entries | map(select(.value.value.lovelace > 25000000)) | .[0].key')

...

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


#---------------------- :plonkupVerifierTx initial transfer: ---------------------

echo "Initial transfer to 'plonkupVerifierTx'..."

in1=$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file  /dev/stdout | jq -r 'to_entries | map(select(.value.value.lovelace > 25000000)) | .[0].key')

...

plonkupVerifierTxTx=$(cardano-cli conway transaction txid --tx-file "$keypath/fundSymb.tx")
plonkupVerifierTxOut=$plonkupVerifierTxTx#0
while true; do
    txOnChain=$(cardano-cli conway query utxo --address $(cat $keypath/plonkupVerifierTx.addr) --testnet-magic $mN --out-file /dev/stdout |
		    jq -r --arg key "$plonkupVerifierTxOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see initial transfer to plonkupVerifierTx..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $plonkupVerifierTxTx"
	break
    fi
done

cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file $assets/utxo1.json
cardano-cli conway query utxo --address $(cat $keypath/plonkupVerifierTx.addr) --testnet-magic $mN --out-file $assets/utxo2.json
plonkupVerifierTxScriptOut=$(cardano-cli conway transaction txid --tx-file "$keypath/parkedScript.tx")#1
cardano-cli conway query utxo --address $(cat $keypath/parkingSpot.addr) --testnet-magic $mN --out-file /dev/stdout |
	   jq -r --arg key "$plonkupVerifierTxScriptOut" 'to_entries | map(select(.key == $key)) | from_entries' > $assets/utxo3.json
cp $keypath/alice.addr $assets/alice.addr

echo ""
echo "Initialization completed."
echo ""
