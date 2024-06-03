#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "someone has seen bob's documents and confirms the transaction."
echo ""

symbolicVerifier=$(cardano-cli transaction txid --tx-file "$keypath/symbolicVerifier.tx")#0
setup=$(cardano-cli transaction txid --tx-file "$keypath/setup.tx")#0
symbolicAlice=$(cardano-cli transaction txid --tx-file "$keypath/symbolicAlice.tx")#1
collateral=$(cardano-cli query utxo --testnet-magic 4 --address $(cat $keypath/someone.addr) --out-file  /dev/stdout | jq -r 'keys[0]')

echo ""
echo "someone address:"
echo "$(cardano-cli query utxo --testnet-magic 4 --address $(cat $keypath/someone.addr))"
echo ""

#------------------------------ :create redeemer: ------------------------------

cardano-cli query utxo \
    --testnet-magic 4 \
    --tx-in "$keypath/setup.tx" \
    --output-json > $assets/setup.json

cardano-cli query utxo \
    --testnet-magic 4 \
    --tx-in "$keypath/symbolicAlice.tx" \
    --output-json > $assets/symbolicAlice.json

cabal run create-contract-transaction

#----------------------------------- :prover: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-file "$keypath/someone.txbody" \
    --tx-in-collateral $collateral \
    --read-only-tx-in-reference $setup \
    --tx-in $symbolicAlice \
    --spending-tx-in-reference $symbolicVerifier \
    --spending-plutus-script-v3 \
    --spending-reference-tx-in-redeemer-file "$assets/redeemerProof.json" \
    --spending-reference-tx-in-inline-datum-present \
    --tx-out "$(cat $keypath/bob.addr) + 50000000 lovelace"
    
cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/someone.txbody" \
    --signing-key-file "$keypath/someone.skey" \
    --out-file "$keypath/someone.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/someone.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""
