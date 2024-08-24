#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets

echo ""
echo "bob burning tokens."
echo ""

in1=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4 --out-file /dev/stdout | jq -r 'to_entries | map(select(.value.value | keys | length > 1)) | .[0].key')
in2=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4 --out-file /dev/stdout | jq -r 'to_entries | map(select((.value.value | keys | length) == 1 and .value.value.lovelace > 10000000)) | .[0].key')
collateral=$in2

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""

plonkVerifier=$(cardano-cli transaction txid --tx-file "$keypath/plonkVerifier.tx")#0
forwardingMintReference=$(cardano-cli transaction txid --tx-file "$keypath/forwardingMint.tx")#0
policyid=$(cardano-cli conway transaction policyid --script-file "$assets/plonkVerifier.plutus")

forwardingMintIn=$(cardano-cli transaction txid --tx-file "$keypath/plonk-transfer.tx")#0

#-------------------------- :tokenname and redeemer: ---------------------------

cabal run plonk-minting-transaction

tokenname=$(head -n 1 "$assets/tokenname" | sed 's/^"//; s/"$//')

redeemerUnit=$assets/unit.json
redeemerProof=$assets/redeemerPlonkVerifier.json
redeemerDummy=$assets/redeemerDummy.json

#---------------------------------- :burning: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --tx-in $in1 \
    --tx-in $in2 \
    --tx-in $forwardingMintIn \
    --spending-tx-in-reference $forwardingMintReference \
    --spending-plutus-script-v3 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file $redeemerUnit \
    --tx-in-collateral $collateral \
    --tx-out "$(cat $keypath/bob.addr) + 10000000 lovelace" \
    --change-address "$(cat $keypath/bob.addr)" \
    --mint "-1 $policyid.$tokenname" \
    --mint-tx-in-reference $plonkVerifier \
    --mint-plutus-script-v3 \
    --mint-reference-tx-in-redeemer-file $redeemerDummy \
    --policy-id $policyid \
    --out-file "$keypath/burning-transaction.txbody"    
    
cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/burning-transaction.txbody" \
    --signing-key-file "$keypath/bob.skey" \
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
