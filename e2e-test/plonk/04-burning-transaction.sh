#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "bob burning tokens."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
collateral=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[1]')

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""

plonkVerifier=$(cardano-cli transaction txid --tx-file "$keypath/plonkVerifier.tx")#0
forwardingMint=$(cardano-cli transaction txid --tx-file "$keypath/forwardingMint.tx")#0
policyid=$(cardano-cli conway transaction policyid --script-file "$assets/plonkVerifier.plutus")

forwardingMintReward=$(cardano-cli transaction txid --tx-file "$keypath/transfer-transaction.tx")#0

#-------------------------- :tokenname and redeemer: ---------------------------

cabal run plonk-minting-transaction

tokenname=$assets/tokenname

redeemerUnit=$assets/unit.json

#---------------------------------- :burning: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/bob.addr)" \
    --tx-in-collateral $collateral \
    --out-file "$keypath/burning-transaction.txbody" \
    --tx-in $in \
    --mint "-1 $policyid.$tokenname" \
    --mint-tx-in-reference $plonkVerifier \
    --mint-plutus-script-v3 \
    --mint-reference-tx-in-redeemer-file $redeemerUnit \
    --policy-id $policyid \
    --tx-in $forwardingMintReward \
    --spending-tx-in-reference $forwardingMint \
    --spending-plutus-script-v3 \
    --spending-reference-tx-in-redeemer-file $redeemerUnit \
    --spending-reference-tx-in-inline-datum-present \
    --tx-out "$(cat $keypath/bob.addr) + 10000000 lovelace"
    
cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/burning-transaction.txbody" \
    --signing-key-file "$keypath/burning-transaction.skey" \
    --out-file "$keypath/burning-transaction.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/burning-transaction.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""
