#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "bob burning tokens."
echo ""

in=$(cardano-cli transaction txid --tx-file "$keypath/minting.tx")#0
collateral=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""

tokenVerifier=$(cardano-cli transaction txid --tx-file "$keypath/tokenVerifier.tx")#0
mintpolicyid=$(cardano-cli conway transaction policyid --script-file "$assets/tokenVerifier.plutus")

#-------------------------- :tokenname and redeemer: ---------------------------

cabal run token-verifier-backend -- burning

tokenname=$assets/tokenname

#---------------------------------- :burning: ----------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/bob.addr)" \
    --tx-in-collateral $collateral \
    --out-file "$keypath/burning.txbody" \
    --tx-in $in \
    --mint "-5 $mintpolicyid.$tokenname" \
    --mint-tx-in-reference $tokenVerifier \
    --mint-plutus-script-v3 \
    --mint-reference-tx-in-redeemer-file "$assets/redeemerVerifierOnlyInput.json" \
    --policy-id $mintpolicyid \
    --tx-out "$(cat $keypath/bob.addr) + 10000000 lovelace"
    
cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/burning.txbody" \
    --signing-key-file "$keypath/burning.skey" \
    --out-file "$keypath/burning.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/burning.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""
