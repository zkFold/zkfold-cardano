#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "alice wants to send money to bob, but only if someone can prove that he is an adult."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
policyid=$(cardano-cli conway transaction policyid --script-file "$assets/plonkVerifier.plutus")

echo "alice address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"
echo ""

#-------------------------------- :create datum: -------------------------------

cabal run symbolic-transfer-transaction -- $policyid

datum=$assets/datumForwardingReward.json

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/alice.addr)" \
    --out-file "$keypath/symbolic-transfer.txbody" \
    --tx-in $in \
    --tx-out "$(cat $keypath/forwardingReward.addr) + 1000000 lovelace" \
    --tx-out-inline-datum-file $datum

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/symbolic-transfer.txbody" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/symbolic-transfer.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/symbolic-transfer.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/symbolic-transfer.tx")"
echo ""
echo "forwardingReward address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/forwardingReward.addr) --testnet-magic 4)"
echo ""
