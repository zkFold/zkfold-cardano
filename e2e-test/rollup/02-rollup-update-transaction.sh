#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets

unitDatum=$assets/unit.cbor
newState=$assets/datumRollup.cbor
rollupRedeemer=$assets/redeemerRollup.cbor
rollupValue=3000000

in1=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
in2=$(cardano-cli transaction txid --tx-file "$keypath/rollupUpdate.tx")#0
collateral=$in1
rollupScript=$(cardano-cli transaction txid --tx-file "$keypath/parkedScript.tx")#0

#---------------------------:new datum & redeemer: -----------------------

cabal run rollup-update-transaction

#------------------------------ :rollup update: --------------------------

echo ""
echo "Rollup update..."
echo ""

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --tx-in $in1 \
  --tx-in $in2 \
  --spending-tx-in-reference $rollupScript \
  --spending-plutus-script-v3 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-cbor-file $rollupRedeemer \
  --tx-in-collateral $collateral \
  --tx-out "$(cat $keypath/rollup.addr) + $rollupValue lovelace" \
  --tx-out-inline-datum-cbor-file $newState \
  --change-address $(cat $keypath/alice.addr) \
  --out-file $keypath/rollupUpdate.txbody

cardano-cli conway transaction sign \
  --testnet-magic 4 \
  --tx-body-file $keypath/rollupUpdate.txbody \
  --signing-key-file $keypath/alice.skey \
  --out-file $keypath/rollupUpdate.tx

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file $keypath/rollupUpdate.tx

echo ""
echo "Pausing for 50 seconds..."
sleep 50

echo ""
echo "Transaction Id: $(cardano-cli transaction txid --tx-file $keypath/rollupUpdate.tx)"
echo ""

mv $assets/nextRedeemerRollup.cbor $assets/redeemerRollup.cbor
mv $assets/nextRedeemerRollup.json $assets/redeemerRollup.json
