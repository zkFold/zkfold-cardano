#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "Alice wants to send money to Bob, but only if he can prove that he is an adult."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo "alice address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"
echo ""

#-------------------------------- :init-script: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/plonkVerify.plutus" \
    --out-file "$keypath/plonkVerify.addr" \
    --testnet-magic 4

#----------------------------------- :sender: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/alice.addr)" \
    --out-file "$keypath/plonkVerify.txbody" \
    --tx-in $in \
    --tx-out "$(cat $keypath/plonkVerify.addr) + 50745828 lovelace" \
    --tx-out-reference-script-file "$assets/plonkVerify.plutus" \
    --tx-out-datum-hash-file "$assets/unit.json"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/plonkVerify.txbody" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/plonkVerify.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/plonkVerify.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/plonkVerify.tx")"
echo ""
echo "script address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/plonkVerify.addr) --testnet-magic 4)"
echo ""
