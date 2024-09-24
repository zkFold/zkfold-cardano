#! /bin/bash

set -e
set -u
set -o pipefail

# SUMMARY: Computing cost for various sripts.

keypath="./scripts/keys"
assetspath="../../assets"

proofRedeemer=$assetspath/redeemerSymbolicVerifier.cbor

#------------------------ :symbolicVerifier: ------------------------

# Plutus script for 'symbolicVerifier' in CBORHex format.
symbolicVerifier_cbor_hex=$(jq -r '.cborHex' $assetspath/symbolicVerifier.plutus)

# TxOutRef for 'symbolicVerifier'
symbolicVerifier=$(cardano-cli transaction txid --tx-file "$keypath/transferScript.tx")#0

# #-------------------------- :'alwaysSucceds': ---------------------------

# in1=$(jq -r '.inputs[0]' $keypath/inputs.json)  # Bob
# in2=$(jq -r '.inputs[1]' $keypath/inputs.json)  # Unit

# cardano-cli conway transaction build \
#   --testnet-magic 4 \
#   --tx-in $in1 \
#   --tx-in $in2 \
#   --tx-in-script-file $assetspath/alwaysSucceeds.plutus \
#   --tx-in-inline-datum-present \
#   --tx-in-redeemer-cbor-file $assetspath/unit.cbor \
#   --tx-in-collateral $in1 \
#   --tx-out "$(cat $keypath/alice.addr) + 1000000 lovelace" \
#   --tx-out "$(cat $keypath/alwaysSucceeds.addr) + 9000000 lovelace" \
#   --tx-out-inline-datum-cbor-file $assetspath/unit.cbor \
#   --change-address "$(cat $keypath/bob.addr)" \
#   --calculate-plutus-script-cost $keypath/alwaysSucceeds.cost

#-------------------------- :'symbolicVerifier': ---------------------------

# in1=$(jq -r '.inputs[0]' $assetspath/inputs.json)  # Bob
# in2=$(jq -r '.inputs[1]' $assetspath/inputs.json)  # symbolicVerifier

in1=$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[1]')  # Bob
in2=$(cardano-cli transaction txid --tx-file "$keypath/fundSymbolic.tx")#0

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --tx-in $in1 \
  --tx-in $in2 \
  --spending-tx-in-reference $symbolicVerifier \
  --spending-plutus-script-v3 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-cbor-file $proofRedeemer \
  --tx-in-collateral $in1 \
  --tx-out "$(cat $keypath/alice.addr) + 1000000 lovelace" \
  --change-address "$(cat $keypath/bob.addr)" \
  --calculate-plutus-script-cost $keypath/symbolicVerifier.cost
