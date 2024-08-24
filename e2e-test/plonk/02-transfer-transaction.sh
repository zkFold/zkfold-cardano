#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets

echo ""
echo "charles wants to create an address with lock reward."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/charles.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
policyid=$(cardano-cli conway transaction policyid --script-file "$assets/plonkVerifier.plutus")

echo "charles address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/charles.addr) --testnet-magic 4)"
echo ""

#-------------------------------- :create datum: -------------------------------

cabal run plonk-transfer-transaction -- $policyid

datum=$assets/datumPlonk.cbor

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/charles.addr)" \
    --out-file "$keypath/plonk-transfer.txbody" \
    --tx-in $in \
    --tx-out "$(cat $keypath/forwardingMint.addr) + 1017160 lovelace" \
    --tx-out-inline-datum-cbor-file $datum

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/plonk-transfer.txbody" \
    --signing-key-file "$keypath/charles.skey" \
    --out-file "$keypath/plonk-transfer.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/plonk-transfer.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 70 seconds..."
echo ""
sleep 70

echo ""
echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/plonk-transfer.tx")"
echo ""
echo "forwardingMint address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/forwardingMint.addr) --testnet-magic 4)"
echo ""
