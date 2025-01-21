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

in=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo "someone address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4)"
echo ""

#-------------------------------- :staking addr: -------------------------------

cardano-cli conway stake-address build \
    --stake-script-file "$assets/plonkVerifierTx.plutus" \
    --out-file "$keypath/plonkVerifierTxStaking.addr" \
    --testnet-magic 4

#------------------------------- :create scripts: -----------------------------

cabal run plonkVerifierTx-init-transaction

#---------------------------- :plonkVerifierTx setup: --------------------------

cardano-cli conway address build \
    --payment-verification-key-file "$keypath/bob.vkey" \
    --stake-script-file "$assets/plonkVerifierTx.plutus" \
    --out-file "$keypath/plonkVerifierTx.addr" \
    --testnet-magic 4

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-file "$keypath/plonkVerifierTx.txbody" \
    --tx-in $in \
    --tx-out "$(cat $keypath/zkfold-main.addr) + 1 lovelace" \
    --tx-out-reference-script-file "$assets/plonkVerifierTx.plutus"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/plonkVerifierTx.txbody" \
    --signing-key-file "$keypath/someone.skey" \
    --out-file "$keypath/plonkVerifierTx.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/plonkVerifierTx.tx"

#----------------------------- :forwarding reward: -----------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/forwardingReward.plutus" \
    --out-file "$keypath/forwardingReward.addr" \
    --testnet-magic 4

#-------------------------------- :send-script: --------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-file "$keypath/forwardingReward.txbody" \
    --tx-in $in 
    --tx-out "$(cat $keypath/zkfold-main.addr) + 1 lovelace" \
    --tx-out-reference-script-file "$assets/forwardingReward.plutus"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/forwardingReward.txbody" \
    --signing-key-file "$keypath/someone.skey" \
    --out-file "$keypath/forwardingReward.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/forwardingReward.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "plonkVerifierTx transaction id: $(cardano-cli conway transaction txid --tx-file "$keypath/plonkVerifierTx.tx")"
echo ""
echo "forwardingReward transaction id: $(cardano-cli conway transaction txid --tx-file "$keypath/forwardingReward.tx")"
echo ""
echo "zkfold-main address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/zkfold-main.addr) --testnet-magic 4)"
echo ""
