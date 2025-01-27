#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

sanchomagic=4
keypath=./plonkupVerifierToken/keys
privpath=./plonkupVerifierToken/priv
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

cabal run plonkupVerifierToken-init-transaction

#-------------------------------- :plonkupVerifierToken setup: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/plonkupVerifierToken.plutus" \
    --out-file "$keypath/plonkupVerifierToken.addr" \
    --testnet-magic $magic

#-------------------------------- :min-required-utxo: --------------------------

cardano-cli conway query protocol-parameters \
  --testnet-magic $magic \
  --out-file $assets/protocol.json

plonkupVerifierSize=$(stat -c%s "$assets/plonkupVerifierToken.plutus")

forwardingMintSize=$(stat -c%s "$assets/forwardingMint.plutus")

plonkupVerifierRequiredUtxo=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $assets/protocol.json \
  --tx-out $(cat $keypath/zkfold-main.addr)+0 \
  --tx-out-reference-script-file "$assets/plonkupVerifierToken.plutus" | sed 's/^[^ ]* //')

forwardingMintRequiredUtxo=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $assets/protocol.json \
  --tx-out $(cat $keypath/zkfold-main.addr)+0 \
  --tx-out-reference-script-file "$assets/forwardingMint.plutus" | sed 's/^[^ ]* //')

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic $magic \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-file "$keypath/plonkupVerifierToken.txbody" \
    --tx-in $in1 \
    --tx-out "$(cat $keypath/zkfold-main.addr) + $plonkupVerifierRequiredUtxo lovelace" \
    --tx-out-reference-script-file "$assets/plonkupVerifierToken.plutus"

cardano-cli conway transaction sign \
    --testnet-magic $magic \
    --tx-body-file "$keypath/plonkupVerifierToken.txbody" \
    --signing-key-file "$keypath/someone.skey" \
    --out-file "$keypath/plonkupVerifierToken.tx"

cardano-cli conway transaction submit \
    --testnet-magic $magic \
    --tx-file "$keypath/plonkupVerifierToken.tx"

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
echo "plonkupVerifierToken transaction id: $(cardano-cli conway transaction txid --tx-file "$keypath/plonkupVerifierToken.tx")"
echo ""
echo "forwardingMint transaction id: $(cardano-cli conway transaction txid --tx-file "$keypath/forwardingMint.tx")"
echo ""
echo "zkfold-main address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/zkfold-main.addr) --testnet-magic $magic)"
echo ""
