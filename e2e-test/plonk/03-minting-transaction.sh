#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets

echo ""
echo "alice minting tokens for bob."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[1]')
collateral=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo ""
echo "alice address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"
echo ""

plonkVerifier=$(cardano-cli transaction txid --tx-file "$keypath/plonkVerifier.tx")#0
policyid=$(cardano-cli conway transaction policyid --script-file "$assets/plonkVerifier.plutus")

#-------------------------- :tokenname and redeemer: ---------------------------

cabal run plonk-minting-transaction

# tokenname=$(head -n 1 $assets/tokenname)
# tokenname=22506f5bb2164d2cd3de3ef082bb9e75a43d1cf0992a0f2f08ca0f52cdbc838f
tokenname=$(head -n 1 "$assets/tokenname" | sed 's/^"//; s/"$//')

redeemerProof=$assets/redeemerPlonkVerifier.json

echo "$policyid.$tokenname"

#---------------------------------- :minting: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --tx-in $in \
    --tx-in-collateral $collateral \
    --tx-out "$(cat $keypath/bob.addr) + 1 lovelace + 1 $policyid.$tokenname" \
    --change-address "$(cat $keypath/alice.addr)" \
    --mint "1 $policyid.$tokenname" \
    --mint-script-file $assets/plonkVerifier.plutus \
    --mint-redeemer-file $redeemerProof \
    --out-file "$keypath/minting-transaction.txbody"

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
