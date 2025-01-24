#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

sanchomagic=4
assets=../assets
keypath=./plonkupVerifierTx-balancing/keys
privpath=./plonkupVerifierTx-balancing/priv

mN=$(cat $privpath/testnet.flag)

mkdir -p $assets

# Wait time (in seconds) before querying blockchain
if [ $mN == $sanchomagic ]; then
    pause=7
else
    pause=4
fi

cabal run balancing-plonkup-verifier

plonkupVerifierTxRedeemer=$assets/redeemerPlonkupVerifierTx.json
plonkupVerifierTxScript=$(cardano-cli conway transaction txid --tx-file "$keypath/parkedScript.tx")#0

plonkupVerifierTxTx=$(cardano-cli conway transaction txid --tx-file "$keypath/fundSymb.tx")
in1=$plonkupVerifierTxTx#1
in2=$plonkupVerifierTxTx#0

ref1=$(cardano-cli conway transaction txid --tx-file "$keypath/parkedScript.tx")#1

cardano-cli conway transaction build \
  --testnet-magic $mN \
  --tx-in $in1 \
  --tx-in $in2 \
  --spending-tx-in-reference $plonkupVerifierTxScript \
  --spending-plutus-script-v3 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $plonkupVerifierTxRedeemer \
  --read-only-tx-in-reference $ref1 \
  --tx-in-collateral $in1 \
  --tx-out "$(cat $keypath/alice.addr) + 10000000 lovelace" \
  --change-address $(cat $keypath/alice.addr) \
  --calculate-plutus-script-cost $keypath/symbolic-exec-units.log

execCpu=$(cat $keypath/symbolic-exec-units.log | jq -r '.[0].executionUnits.steps')
execMem=$(cat $keypath/symbolic-exec-units.log | jq -r '.[0].executionUnits.memory')

echo ""
echo "Execution step units:   $execCpu"
echo "Execution memory units: $execMem"
echo ""
