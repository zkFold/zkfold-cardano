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
echo "charles wants to create an address with lock reward."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/charles.addr) --testnet-magic $magic --out-file  /dev/stdout | jq -r 'keys[0]')
policyid=$(cardano-cli conway transaction policyid --script-file "$assets/plonkVerifierToken.plutus")

echo "charles address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/charles.addr) --testnet-magic $magic)"
echo ""

#-------------------------------- :create datum: -------------------------------

cabal run plonkVerifierToken-transfer-transaction -- $policyid

datum=$assets/datumPlonkVerifierToken.cbor

forwardingMintDatumReqUtxo=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $assets/protocol.json \
  --tx-out $(cat $keypath/forwardingMint.addr)+0 \
  --tx-out-inline-datum-cbor-file $datum | sed 's/^[^ ]* //')

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic $magic \
    --change-address "$(cat $keypath/charles.addr)" \
    --out-file "$keypath/plonkVerifierToken-transfer.txbody" \
    --tx-in $in \
    --tx-out "$(cat $keypath/forwardingMint.addr) + $forwardingMintDatumReqUtxo lovelace" \
    --tx-out-inline-datum-cbor-file $datum

cardano-cli conway transaction sign \
    --testnet-magic $magic \
    --tx-body-file "$keypath/plonkVerifierToken-transfer.txbody" \
    --signing-key-file "$keypath/charles.skey" \
    --out-file "$keypath/plonkVerifierToken-transfer.tx"

cardano-cli conway transaction submit \
    --testnet-magic $magic \
    --tx-file "$keypath/plonkVerifierToken-transfer.tx"

#-------------------------------------------------------------------------------

if [ $magic == $sanchomagic ]; then
    pause=75
else
    pause=5
fi

echo ""
echo "Pausing for $pause seconds..."
echo ""
sleep $pause

echo ""
echo "transaction id: $(cardano-cli transaction txid --tx-file "$keypath/plonkVerifierToken-transfer.tx")"
echo ""
echo "forwardingMint address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/forwardingMint.addr) --testnet-magic $magic)"
echo ""
