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
echo "someone wants to create an address with scripts."
echo ""

in1=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic $magic --out-file  /dev/stdout | jq -r 'keys[0]')
in2=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic $magic --out-file  /dev/stdout | jq -r 'keys[1]')

echo "someone address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic $magic)"
echo ""

#------------------------------- :create scripts: ------------------------------

cabal run plonk-init-transaction

#-------------------------------- :plonk setup: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/plonkVerifier.plutus" \
    --out-file "$keypath/plonkVerifier.addr" \
    --testnet-magic $magic

#-------------------------------- :min-required-utxo: --------------------------

cardano-cli conway query protocol-parameters \
  --testnet-magic $magic \
  --out-file $assets/protocol.json

plonkVerifierSize=$(stat -c%s "$assets/plonkVerifier.plutus")

forwardingMintSize=$(stat -c%s "$assets/forwardingMint.plutus")

plonkVerifierRequiredUtxo=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $assets/protocol.json \
  --tx-out $(cat $keypath/zkfold-main.addr)+0 \
  --tx-out-reference-script-file "$assets/plonkVerifier.plutus" | sed 's/^[^ ]* //')

forwardingMintRequiredUtxo=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $assets/protocol.json \
  --tx-out $(cat $keypath/zkfold-main.addr)+0 \
  --tx-out-reference-script-file "$assets/forwardingMint.plutus" | sed 's/^[^ ]* //')

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic $magic \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-file "$keypath/plonkVerifier.txbody" \
    --tx-in $in1 \
    --tx-out "$(cat $keypath/zkfold-main.addr) + $plonkVerifierRequiredUtxo lovelace" \
    --tx-out-reference-script-file "$assets/plonkVerifier.plutus"

cardano-cli conway transaction sign \
    --testnet-magic $magic \
    --tx-body-file "$keypath/plonkVerifier.txbody" \
    --signing-key-file "$keypath/someone.skey" \
    --out-file "$keypath/plonkVerifier.tx"

cardano-cli conway transaction submit \
    --testnet-magic $magic \
    --tx-file "$keypath/plonkVerifier.tx"

echo ""

#------------------------------ :forwarding mint: ------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/forwardingMint.plutus" \
    --out-file "$keypath/forwardingMint.addr" \
    --testnet-magic $magic

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic $magic \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-file "$keypath/forwardingMint.txbody" \
    --tx-in $in2 \
    --tx-out "$(cat $keypath/zkfold-main.addr) + $forwardingMintRequiredUtxo lovelace" \
    --tx-out-reference-script-file "$assets/forwardingMint.plutus"

cardano-cli conway transaction sign \
    --testnet-magic $magic \
    --tx-body-file "$keypath/forwardingMint.txbody" \
    --signing-key-file "$keypath/someone.skey" \
    --out-file "$keypath/forwardingMint.tx"

cardano-cli conway transaction submit \
    --testnet-magic $magic \
    --tx-file "$keypath/forwardingMint.tx"

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
echo "plonkVerifier transaction id: $(cardano-cli transaction txid --tx-file "$keypath/plonkVerifier.tx")"
echo ""
echo "forwardingMint transaction id: $(cardano-cli transaction txid --tx-file "$keypath/forwardingMint.tx")"
echo ""
echo "zkfold-main address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/zkfold-main.addr) --testnet-magic $magic)"
echo ""
