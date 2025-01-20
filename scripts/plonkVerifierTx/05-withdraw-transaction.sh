#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../assets

echo ""
echo "bob withdrawal ada."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
collateral=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[1]')

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --testnet-magic 4 --address $(cat $keypath/bob.addr))"
echo ""

plonkVerifierTx=$(cardano-cli transaction txid --tx-file "$keypath/plonkVerifierTx.tx")#0
forwardingRewardReference=$(cardano-cli transaction txid --tx-file "$keypath/forwardingMint.tx")#0
base16=$(cardano-cli conway address info --address "$keypath/plonkVerifierTx.addr" /dev/stdout | jq -r 'keys[0]')

forwardingRewardIn=$(cardano-cli transaction txid --tx-file "$keypath/plonk-transfer.tx")#0

#---------------------------------- :redeemer: ---------------------------------

cabal run plonkVerifierTx-withdraw-transaction -- $base16

plutusStakingScriptRedeemer=$assets/redeemerPlonkVerifierTx.json

redeemerUnit=$assets/unit.json

#---------------------------------- :withdraw: ---------------------------------

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/bob.addr)" \
    --tx-in-collateral $collateral \
    --protocol-params-file "$keypath/pparams.json" \
    --out-file "$keypath/staking-script-withdrawal.txbody" \
    --tx-in $in \
    --withdrawal "$(cat $keypath/plonkVerifierTxStaking.addr) + 0 lovelace" \
    --withdrawal-plutus-script-v3 \
    --withdrawal-tx-in-reference "$assets/plonkVerifierTx.plutus" \
    --withdrawal-reference-tx-in-redeemer-file $plutusStakingScriptRedeemer \
    --tx-in $forwardingRewardIn \
    --spending-tx-in-reference $forwardingRewardReference \
    --spending-plutus-script-v3 \
    --spending-reference-tx-in-redeemer-file $redeemerUnit \
    --spending-reference-tx-in-inline-datum-present \
    --tx-out "$(cat $keypath/bob.addr) + 10000000 lovelace"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/staking-script-withdrawal.txbody" \
    --signing-key-file "$keypath/bob.skey" \
    --out-file "$keypath/staking-script-withdrawal.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/taking-script-withdrawal.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""
