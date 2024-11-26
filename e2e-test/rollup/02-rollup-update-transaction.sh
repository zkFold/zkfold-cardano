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

stateA=$assets/datumA.cbor
rollupRedeemerA=$assets/rollupRedeemerA.cbor

rollupScript=$(cardano-cli transaction txid --tx-file "$keypath/parkedScript.tx")#0
rollupScriptFile=$assets/rollup.plutus

rollupLovelaceValue=3000000  # Value (in lovelaces) to be transfered with each rollup
feeValue=15000000  # rollup fee

nftPolicy=$assets/nftPolicy.plutus
nftPolicyId=$(cardano-cli conway transaction policyid --script-file $nftPolicy)

#-------------------------------- :rollup tx: -------------------------------

echo ""
echo "Starting next rollup update..."
echo ""

# cabal run rollup-update-loop

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file  /dev/stdout | jq -r 'keys[0]')
inRB=$(cardano-cli transaction txid --tx-file "$keypath/rollupOutB.tx")#0
inData=$(cardano-cli transaction txid --tx-file "$keypath/dataRef.tx")#0

cardano-cli conway transaction build \
  --testnet-magic $mN \
  --tx-in $in1 \
  --tx-in $inRB \
  --spending-tx-in-reference $rollupScript \
  --spending-plutus-script-v3 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-cbor-file $rollupRedeemerA \
  --read-only-tx-in-reference $inData \
  --tx-in-collateral $in1 \
  --tx-out "$(cat $keypath/rollup.addr) + $rollupLovelaceValue lovelace + 1 $nftPolicyId.7a6b466f6c64" \
  --tx-out-inline-datum-cbor-file $stateA \
  --tx-out "$(cat $keypath/alice.addr) + $feeValue lovelace" \
  --change-address $(cat $keypath/alice.addr) \
  --out-file $keypath/rollupOutA.txbody

cardano-cli conway transaction sign \
  --testnet-magic $mN \
  --tx-body-file $keypath/rollupOutA.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/rollupOutA.tx

cardano-cli conway transaction submit \
  --testnet-magic $mN \
  --tx-file $keypath/rollupOutA.tx

rollupTx=$(cardano-cli transaction txid --tx-file "$keypath/rollupOutA.tx")
rollupOut=$rollupTx#0
while true; do
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/rollup.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$rollupOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see rollup tx onchain..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $rollupTx"
	break
    fi
done

echo ""
echo "One rollup transaction completed."
echo ""
