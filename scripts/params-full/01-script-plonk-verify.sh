#! /bin/bash

LOVELACE=1000000

keypath=../keys

# Build output address 
cardano-cli conway address build \
    --payment-script-file "../assets/plonkVerify.plutus" \
    --testnet-magic 4 \
    --out-file "$keypath/plonk-verify-output.addr"

in=$(cardano-cli query utxo --address $(cat $keypath/plonk-verify-input.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

# Build the transaction
cardano-cli conway transaction build \
    --testnet-magic 4 \
    --tx-in $in \
    --tx-out "$(cat "$keypath/plonk-verify-output.addr") + 3000000 lovelace" \
    --change-address "$(cat "$keypath/plonk-verify-input.addr")" \
    --out-file "$keypath/plonk-verify-output.txbody"
    
# Sign the transaction
cardano-cli conway transaction sign \
    --tx-body-file "$keypath/plonk-verify-output.txbody" \
    --signing-key-file "$keypath/plonk-verify-input.skey" \
    --testnet-magic 4 \
    --out-file "$keypath/plonk-verify-output.tx"

# Submit the transaction
cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/plonk-verify-output.tx"

echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/plonk-verify-output.tx")"

echo "out address: $(cardano-cli query utxo --address $(cat $keypath/plonk-verify-output.addr) --testnet-magic 4)"
