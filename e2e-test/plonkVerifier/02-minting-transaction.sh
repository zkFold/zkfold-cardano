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

tokenVerifier=$(cardano-cli transaction txid --tx-file "$keypath/tokenVerifier.tx")#0
mintpolicyid=$(cardano-cli conway transaction policyid --script-file "$assets/tokenVerifier.plutus")

#-------------------------- :tokenname and redeemer: ---------------------------

cabal run token-verifier-backend -- minting

tokenname=$assets/tokenname

#---------------------------------- :minting: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/alice.addr)" \
    --tx-in-collateral $collateral \
    --out-file "$keypath/minting.txbody" \
    --tx-in $in \
    --mint "5 $mintpolicyid.$tokenname" \
    --mint-tx-in-reference $tokenVerifier \
    --mint-plutus-script-v3 \
    --mint-reference-tx-in-redeemer-file "$assets/redeemerVerifier.json" \
    --policy-id $mintpolicyid \
    --tx-out "$(cat $keypath/bob.addr) + 10000000 lovelace + 5 $mintpolicyid.$tokenname"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/minting.txbody" \
    --signing-key-file "$keypath/alice.skey" \
    --out-file "$keypath/minting.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/minting.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/tokenVerifier.tx")"
echo ""
echo "script address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/plonkVerify.addr) --testnet-magic 4)"
echo ""
