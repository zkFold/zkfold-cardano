#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "zkfold wants to create an address with script."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/zkfold-main.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo "zkfold-main address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/zkfold-main.addr) --testnet-magic 4)"
echo ""

#-------------------------------- :init-script: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/tokenVerifier.plutus" \
    --out-file "$keypath/tokenVerifier.addr" \
    --testnet-magic 4

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/zkfold-main.addr)" \
    --out-file "$keypath/tokenVerifier.txbody" \
    --tx-in $in \
    --tx-out "$(cat $keypath/tokenVerifier.addr) + 1000000 lovelace" \
    --tx-out-reference-script-file "$assets/tokenVerifier.plutus"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/tokenVerifier.txbody" \
    --signing-key-file "$keypath/zkfold-main.skey" \
    --out-file "$keypath/tokenVerifier.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/tokenVerifier.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/tokenVerifier.tx")"
echo ""
echo "script address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/tokenVerifier.addr) --testnet-magic 4)"
echo ""
