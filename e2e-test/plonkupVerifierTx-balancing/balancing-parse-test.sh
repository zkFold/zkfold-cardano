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

cabal run balancing-parse-input

plonkupVerifierTxRedeemer=$assets/redeemerPlonkupVerifierTx.json
plonkupVerifierTxScript=$(cardano-cli conway transaction txid --tx-file "$keypath/parkedScript.tx")#0

plonkupVerifierTxTx=$(cardano-cli conway transaction txid --tx-file "$keypath/fundSymb.tx")
in1=$plonkupVerifierTxTx#1
in2=$plonkupVerifierTxTx#0

echo ""
echo "Retrieving funds from PlonkupVerifierTx..."
echo ""

cardano-cli conway transaction build \
  --testnet-magic $mN \
  --tx-in $in1 \
  --tx-in $in2 \
  --spending-tx-in-reference $plonkupVerifierTxScript \
  --spending-plutus-script-v3 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $plonkupVerifierTxRedeemer \
  --tx-in-collateral $in1 \
  --tx-out "$(cat $keypath/alice.addr) + 10000000 lovelace" \
  --change-address $(cat $keypath/alice.addr) \
  --out-file $keypath/retrieveSymb.txbody

cardano-cli conway transaction sign \
  --testnet-magic $mN \
  --tx-body-file $keypath/retrieveSymb.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/retrieveSymb.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/retrieveSymb.tx

retrieveTx=$(cardano-cli conway transaction txid --tx-file "$keypath/retrieveSymb.tx")
retrieveOut=$retrieveTx#0
while true; do
    txOnChain=$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file /dev/stdout |
		    jq -r --arg key "$retrieveOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see fund retrieving from PlonkupVerifierTx..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $retrieveTx"
	break
    fi
done

echo ""
echo "Fund retrieving completed."
echo ""
