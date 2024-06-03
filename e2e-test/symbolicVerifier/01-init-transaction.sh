#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "zkfold wants to create an address with setup in datum and also create an address with script."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/zkfold-main.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo "zkfold-main address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/zkfold-main.addr) --testnet-magic 4)"
echo ""

#-------------------------------- :init-script: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/symbolicVerifier.plutus" \
    --out-file "$keypath/symbolicVerifier.addr" \
    --testnet-magic 4

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/zkfold-main.addr)" \
    --out-file "$keypath/symbolicVerifier.txbody" \
    --tx-in $in \
    --tx-out "$(cat $keypath/symbolicVerifier.addr) + 1000000 lovelace" \
    --tx-out-reference-script-file "$assets/symbolicVerifier.plutus" \
    --tx-out-datum-hash-file "$assets/always-false-datum.json"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/symbolicVerifier.txbody" \
    --signing-key-file "$keypath/zkfold-main.skey" \
    --out-file "$keypath/symbolicVerifier.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/symbolicVerifier.tx"

#-------------------------------- :init-setup: ---------------------------------

cabal run create-init-transaction

sleep 5

#-------------------------------- :send-setup: ---------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/zkfold-main.addr)" \
    --out-file "$keypath/setup.txbody" \
    --tx-in $in \
    --tx-out "$(cat $keypath/zkfold-setup.addr) + 1000000 lovelace" \
    --tx-out-datum-hash-file "$assets/setup.json"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/setup.txbody" \
    --signing-key-file "$keypath/zkfold-main.skey" \
    --out-file "$keypath/setup.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/setup.tx"

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

echo ""
echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/setup.tx")"
echo ""
echo "script address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/zkfold-setup.addr) --testnet-magic 4)"
echo ""
