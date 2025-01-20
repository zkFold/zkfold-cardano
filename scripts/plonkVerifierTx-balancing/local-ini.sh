#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

localmagic=42
pause=4
assets=../assets
keypath0=./local-testnet/example/utxo-keys
keypath=./plonkVerifierTx-balancing/keys
privpath=./plonkVerifierTx-balancing/priv

if [ -d "./plonkVerifierTx-balancing" ]; then
    mkdir -p $assets
    mkdir -p $keypath
    mkdir -p $privpath
else
    echo "Please run script from directory 'e2e-test'."
    exit 1
fi

printf "$localmagic" > $privpath/testnet.flag
mN=$localmagic

./plonkVerifierTx-balancing/init-alice.sh

echo "Now funding Alice..."

#----------------------------------- :utxo1: -----------------------------------

cardano-cli conway address build \
  --payment-verification-key-file $keypath0/utxo1.vkey \
  --out-file $keypath0/utxo1.addr \
  --testnet-magic $mN

#----------------------------------- :funding: -----------------------------------

in1=$(cardano-cli query utxo --address $(cat $keypath0/utxo1.addr) --testnet-magic $mN --out-file  /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
  --tx-in $in1 \
  --tx-out "$(cat $keypath/alice.addr) + 1000000000" \
  --change-address $(cat $keypath0/utxo1.addr) \
  --out-file $keypath/tx.body \
  --testnet-magic $mN

cardano-cli conway transaction sign \
  --tx-body-file $keypath/tx.body \
  --signing-key-file $keypath0/utxo1.skey \
  --out-file $keypath/tx.signed \
  --testnet-magic $mN

cardano-cli conway transaction submit \
  --tx-file $keypath/tx.signed \
  --testnet-magic $mN

lastTx=$(cardano-cli transaction txid --tx-file "$keypath/tx.signed")
lastTxOut=$lastTx#0

while true; do
    txOnChain=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$lastTxOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see initial funding tx onchain..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $lastTx"
	break
    fi
done

echo ""
echo "Alice's wallet:"
echo "$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN)"
echo ""
echo "Done."
echo ""
