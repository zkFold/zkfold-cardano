#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets

pause=7
mN=42

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file  /dev/stdout | jq -r 'keys[0]')
alice0=$(cardano-cli query utxo --address $(cat ./keys/alice.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r '. | to_entries[0].value.value.lovelace')

echo "Split Alice's funds (at UTxO #0) into two separate UTxO's..."

cardano-cli conway transaction build \
  --testnet-magic $mN \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/alice.addr) + $((alice0/2)) lovelace" \
  --change-address $(cat $keypath/alice.addr) \
  --out-file $keypath/splitAlice.txbody

cardano-cli conway transaction sign \
  --testnet-magic $mN \
  --tx-body-file $keypath/splitAlice.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/splitAlice.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/splitAlice.tx

splitTx=$(cardano-cli transaction txid --tx-file "$keypath/splitAlice.tx")
splitOut=$splitTx#0
while true; do
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$splitOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see splitting tx onchain..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $splitTx"
	break
    fi
done

echo ""
echo "Done."
echo ""
