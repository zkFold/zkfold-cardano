#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "charles wants to create an address with lock reward."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/charles.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo "charles address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/charles.addr) --testnet-magic 4)"
echo ""


#-------------------------------- :create datum: -------------------------------

cabal run plonk-transfer-transaction

datum=$assets/forwardingMint-datum.json

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/charles.addr)" \
    --out-file "$keypath/transfer-transaction.txbody" \
    --tx-in $in \
    --tx-out "$(cat $keypath/forwardingMint.addr) + 1000000 lovelace" \
    --tx-out-inline-datum-file $datum

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/transfer-transaction.txbody" \
    --signing-key-file "$keypath/charles.skey" \
    --out-file "$keypath/transfer-transaction.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/transfer-transaction.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/transfer-transaction.tx")"
echo ""
echo "forwardingMint address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/forwardingMint.addr) --testnet-magic 4)"
echo ""
