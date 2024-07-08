#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "alice minting tokens for bob."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
collateral=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo ""
echo "alice address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"
echo ""

plonkVerifier=$(cardano-cli transaction txid --tx-file "$keypath/plonkVerifier.tx")#0
policyid=$(cardano-cli conway transaction policyid --script-file "$assets/plonkVerifier.plutus")

#-------------------------- :tokenname and redeemer: ---------------------------

cabal run plonk-minting-transaction

tokenname=$assets/tokenname

redeemerProof=$assets/redeemerVerifier.json

#---------------------------------- :minting: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/alice.addr)" \
    --tx-in-collateral $collateral \
    --out-file "$keypath/minting-transaction.txbody" \
    --tx-in $in \
    --mint "1 $policyid.$tokenname" \
    --mint-tx-in-reference $plonkVerifier \
    --mint-plutus-script-v3 \
    --mint-reference-tx-in-redeemer-file $redeemerProof \
    --policy-id $policyid \
    --tx-out "$(cat $keypath/bob.addr) + 1 lovelace + 1 $policyid.$tokenname"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/minting-transaction.txbody" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/minting-transaction.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/minting-transaction.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/plonkVerifier.tx")"
echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""
