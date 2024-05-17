#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "alice wants to send money to bob, but only if someone can prove that he is an adult."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo "alice address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"
echo ""

#-------------------------------- :init-script: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/symbolicVerifier.plutus" \
    --out-file "$keypath/symbolicVerifier.addr" \
    --testnet-magic 4

#----------------------------------- :sender: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --tx-in $in \
    --tx-out "$(cat $keypath/symbolicVerifier.addr) + 50745828 lovelace" \
    --change-address "$(cat $keypath/alice.addr)" \
    --tx-out-reference-script-file "$assets/symbolicVerifier.plutus" \
    --tx-out-datum-hash-file "$assets/datum.json" \
    --out-file "$keypath/symbolicVerifier.txbody"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/symbolicVerifier.txbody" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/symbolicVerifier.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/symbolicVerifier.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/symbolicVerifier.tx")"
echo ""
echo "script address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/symbolicVerifier.addr) --testnet-magic 4)"
echo ""
