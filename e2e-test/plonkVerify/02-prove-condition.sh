#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "Bob confirms the transaction."
echo ""

script=$(cardano-cli transaction txid --tx-file "$keypath/plonkVerify.tx")#0
collateral=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
reference=$collateral

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""

#----------------------------------- :prover: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/bob.addr)" \
    --tx-in-collateral $collateral \
    --out-file "$keypath/bob.txbody" \
    --tx-in $script \
    --spending-tx-in-reference $reference \
    --spending-plutus-script-v3 \
    --spending-reference-tx-in-redeemer-file "$assets/redeemer.json" \
    --spending-reference-tx-in-datum-file "$assets/unit.json" \
    --tx-out "$(cat $keypath/bob.addr) + 50000000 lovelace"
    
cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/bob.txbody" \
    --signing-key-file "$keypath/bob.skey" \
    --out-file "$keypath/bob.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/bob.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""
