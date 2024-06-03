#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "alice wants to send money to bob, but only if someone can prove that he is an adult."
echo ""

symbolicVerifier=$(cardano-cli transaction txid --tx-file "$keypath/symbolicVerifier.tx")#0
setup=$(cardano-cli transaction txid --tx-file "$keypath/setup.tx")#0
alice=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
collateral=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo "alice address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"
echo ""

#------------------------------ :hash transaction: -----------------------------

cabal run create-transfer-transaction -- conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/someone.addr)" \
    --tx-in-collateral $collateral \
    --out-file "$keypath/someone.txbody" \
    --tx-in $alice \
    --tx-in $setup \
    --tx-in $symbolicVerifier \
    --tx-out "$(cat $keypath/bob.addr) + 50000000 lovelace"

#----------------------------------- :sender: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/alice.addr)" \
    --out-file "$keypath/symbolicAlice.txbody" \
    --tx-in $alice \
    --tx-out "$(cat $keypath/symbolicVerifier.addr) + 50745828 lovelace" \
    --tx-out-datum-hash-file "$assets/datumAlice.json"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/symbolicAlice.txbody" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/symbolicAlice.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/symbolicAlice.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/symbolicAlice.tx")"
echo ""
echo "script address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/symbolicVerifier.addr) --testnet-magic 4)"
echo ""
