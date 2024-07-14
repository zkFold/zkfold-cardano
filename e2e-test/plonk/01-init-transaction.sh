#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../assets

echo ""
echo "someone wants to create an address with scripts."
echo ""

in1=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
in2=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[1]')

echo "someone address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4)"
echo ""

#------------------------------- :create scripts: ------------------------------

cabal run plonk-init-transaction

#-------------------------------- :plonk setup: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/plonkVerifier.plutus" \
    --out-file "$keypath/plonkVerifier.addr" \
    --testnet-magic 4

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-file "$keypath/plonkVerifier.txbody" \
    --tx-in $in1 \
    --tx-out "$(cat $keypath/zkfold-main.addr) + 1 lovelace" \
    --tx-out-reference-script-file "$assets/plonkVerifier.plutus"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/plonkVerifier.txbody" \
    --signing-key-file "$keypath/someone.skey" \
    --out-file "$keypath/plonkVerifier.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/plonkVerifier.tx"

#------------------------------ :forwarding mint: ------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/forwardingMint.plutus" \
    --out-file "$keypath/forwardingMint.addr" \
    --testnet-magic 4

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-file "$keypath/forwardingMint.txbody" \
    --tx-in $in2 \
    --tx-out "$(cat $keypath/zkfold-main.addr) + 1 lovelace" \
    --tx-out-reference-script-file "$assets/forwardingMint.plutus"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/forwardingMint.txbody" \
    --signing-key-file "$keypath/someone.skey" \
    --out-file "$keypath/forwardingMint.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/forwardingMint.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "plonkVerifier transaction id: $(cardano-cli transaction txid --tx-file "$keypath/plonkVerifier.tx")"
echo ""
echo "forwardingMint transaction id: $(cardano-cli transaction txid --tx-file "$keypath/forwardingMint.tx")"
echo ""
echo "zkfold-main address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/zkfold-main.addr) --testnet-magic 4)"
echo ""
