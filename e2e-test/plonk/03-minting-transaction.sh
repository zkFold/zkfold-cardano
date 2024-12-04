#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

sanchomagic=4
keypath=./plonk/keys
privpath=./plonk/priv
assets=../assets

magic=$(cat $privpath/testnet.flag)

echo ""
echo "alice minting tokens for bob."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $magic --out-file  /dev/stdout | jq -r 'keys[0]')
collateral=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $magic --out-file  /dev/stdout | jq -r 'keys[0]')

echo ""
echo "alice address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic $magic)"
echo ""

plonkVerifierToken=$(cardano-cli transaction txid --tx-file "$keypath/plonkVerifierToken.tx")#0
policyid=$(cardano-cli conway transaction policyid --script-file "$assets/plonkVerifierToken.plutus")

#-------------------------- :tokenname and redeemer: ---------------------------

cabal run plonk-minting-transaction

tokenname=$(head -n 1 "$assets/tokenname" | sed 's/^"//; s/"$//')

redeemerProof=$assets/redeemerPlonkVerifierToken.cbor

echo "PolicyID & TokenName:"
echo "$policyid.$tokenname"

#---------------------------------- :minting: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic $magic \
    --change-address "$(cat $keypath/alice.addr)" \
    --tx-in-collateral $collateral \
    --out-file "$keypath/minting-transaction.txbody" \
    --tx-in $in \
    --mint "1 $policyid.$tokenname" \
    --mint-tx-in-reference $plonkVerifierToken \
    --mint-plutus-script-v3 \
    --mint-reference-tx-in-redeemer-cbor-file $redeemerProof \
    --policy-id $policyid \
    --tx-out "$(cat $keypath/bob.addr) + 1142150 lovelace + 1 $policyid.$tokenname"

cardano-cli conway transaction sign \
    --testnet-magic $magic \
    --tx-body-file "$keypath/minting-transaction.txbody" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/minting-transaction.tx"

cardano-cli conway transaction submit \
    --testnet-magic $magic \
    --tx-file "$keypath/minting-transaction.tx"

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
echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/minting-transaction.tx")"
echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic $magic)"
echo ""
