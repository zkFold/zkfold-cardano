#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

sanchomagic=4
keypath=./plonkVerifierToken/keys
privpath=./plonkVerifierToken/priv
assets=../assets

magic=$(cat $privpath/testnet.flag)

echo ""
echo "bob burning tokens."
echo ""

in1=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic --out-file /dev/stdout | jq -r 'to_entries | map(select(.value.value | keys | length > 1)) | .[0].key')
in2=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic --out-file /dev/stdout | jq -r 'to_entries | map(select((.value.value | keys | length) == 1 and .value.value.lovelace > 10000000)) | .[0].key')
collateral=$in2

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic)"
echo ""

plonkVerifierToken=$(cardano-cli conway transaction txid --tx-file "$keypath/plonkVerifierToken.tx")#0
forwardingMintReference=$(cardano-cli conway transaction txid --tx-file "$keypath/forwardingMint.tx")#0
policyid=$(cardano-cli conway transaction policyid --script-file "$assets/plonkVerifierToken.plutus")

forwardingMintIn=$(cardano-cli conway transaction txid --tx-file "$keypath/plonkVerifierToken-transfer.tx")#0

#-------------------------- :tokenname and redeemer: ---------------------------

cabal run plonkVerifierToken-minting-transaction

tokenname=$(head -n 1 "$assets/tokenname" | sed 's/^"//; s/"$//')

redeemerUnit=$assets/unit.cbor
redeemerDummy=$assets/dummy-redeemer.cbor

#---------------------------------- :burning: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic $magic \
    --tx-in $in1 \
    --tx-in $in2 \
    --tx-in $forwardingMintIn \
    --spending-tx-in-reference $forwardingMintReference \
    --spending-plutus-script-v3 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-cbor-file $redeemerUnit \
    --tx-in-collateral $collateral \
    --tx-out "$(cat $keypath/bob.addr) + 10000000 lovelace" \
    --change-address "$(cat $keypath/bob.addr)" \
    --mint "-1 $policyid.$tokenname" \
    --mint-tx-in-reference $plonkVerifierToken \
    --mint-plutus-script-v3 \
    --mint-reference-tx-in-redeemer-cbor-file $redeemerDummy \
    --policy-id $policyid \
    --out-file "$keypath/burning-transaction.txbody"    
    
cardano-cli conway transaction sign \
    --testnet-magic $magic \
    --tx-body-file "$keypath/burning-transaction.txbody" \
    --signing-key-file "$keypath/bob.skey" \
    --out-file "$keypath/burning-transaction.tx"

cardano-cli conway transaction submit \
    --testnet-magic $magic \
    --tx-file "$keypath/burning-transaction.tx"

#-------------------------------------------------------------------------------

if [ $magic == $sanchomagic ]; then
    pause=60
else
    pause=5
fi

echo ""
echo "Pausing for $pause seconds..."
echo ""
sleep $pause

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic)"
echo ""
