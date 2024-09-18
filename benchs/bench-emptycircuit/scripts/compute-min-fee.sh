#! /bin/bash

set -e
set -u
set -o pipefail

# SUMMARY: This script computes the 'minFee' for executing 'symbolicVerifier' on
# input data stored in file 'inputs.json'.

keypath="./scripts/keys"
assetspath="../../assets"

unitRedeemer=$assetspath/unit.cbor
proofRedeemer=$assetspath/genericRedeemer.cbor

#------------------------ :symbolicVerifier: ------------------------

# Plutus script for 'symbolicVerifier' in CBORHex format.
symbolicVerifier_cbor_hex=$(jq -r '.cborHex' $assetspath/symbolicVerifier.plutus)

# TxOutRef for 'symbolicVerifier'
symbolicVerifier=$(cardano-cli transaction txid --tx-file "$keypath/transferScript.tx")#0

# Execution units for 'alwaysSucceeds' (parking script for 'symbolicVerifier' in example)
alwaysSucceedsExecUnits="(2164, 439096)"

# #-------------------------- :inputs size: ---------------------------

# input_tx_out_refs=($(jq -r '.inputs[]' $assetspath/inputs.json))

# total_inputs_size=0

# for oref in "${input_tx_out_refs[@]}"; do
#     # Query UTxO data for current oref
#     query=$(cardano-cli conway query utxo --address $(cat $keypath/alwaysSucceeds.addr) --testnet-magic 4 --out-file /dev/stdout | jq -r --arg key "$oref" '.[$key]')

#     # Extract inlineDatum and inlineDatumhash
#     inline_datum_hex=$(echo "$query" | jq -r '.inlineDatum.bytes // empty')
#     inline_datumhash=$(echo "$query" | jq -r '.inlineDatumhash // empty')

#     # Size of inlineDatum in bytes
#     inline_datum_bytes=0
#     if [ -n "$inline_datum_hex" ]; then
#         inline_datum_bytes=$(echo -n "$inline_datum_hex" | wc -c)
#         inline_datum_bytes=$((inline_datum_bytes / 2))
#     fi

#     # Size of inlineDatumhash in bytes
#     inline_datumhash_bytes=0
#     if [ -n "$inline_datumhash" ]; then
#         inline_datumhash_bytes=$(echo -n "$inline_datumhash" | wc -c)
#         inline_datumhash_bytes=$((inline_datumhash_bytes / 2))
#     fi

#     # Remove inlineDatum and inlineDatumhash fields for remaining size calculation
#     input_without_bytes=$(echo "$query" | jq -r 'del(.inlineDatum, .inlineDatumhash)')

#     # Calculate the size of the rest of the JSON structure
#     input_size=$(echo "$input_without_bytes" | jq -c '.' | wc -c)

#     # Calculate the total size in bytes for this TxOutRef
#     txoutref_size=$((input_size + inline_datum_bytes + inline_datumhash_bytes))

#     echo "Estimated size for input $oref: $txoutref_size bytes."

#     # Add the size of this TxOutRef to the total size
#     total_inputs_size=$((total_inputs_size + txoutref_size))

# done

# #-------------------------- :references size: -------------------------

# reference_tx_out_refs=($(jq -r '."reference-inputs"[]' $assetspath/inputs.json))

# total_references_size=0

# for oref in "${reference_tx_out_refs[@]}"; do
#     # Query UTxO data for current oref
#     query=$(cardano-cli conway query utxo --address $(cat $keypath/alwaysSucceeds.addr) --testnet-magic 4 --out-file /dev/stdout | jq -r --arg key "$oref" '.[$key]')

#     # Extract inlineDatum and inlineDatumhash
#     inline_datum_hex=$(echo "$query" | jq -r '.inlineDatum.bytes // empty')
#     inline_datumhash=$(echo "$query" | jq -r '.inlineDatumhash // empty')

#     # Size of inlineDatum in bytes
#     inline_datum_bytes=0
#     if [ -n "$inline_datum_hex" ]; then
#         inline_datum_bytes=$(echo -n "$inline_datum_hex" | wc -c)
#         inline_datum_bytes=$((inline_datum_bytes / 2))
#     fi

#     # Size of inlineDatumhash in bytes
#     inline_datumhash_bytes=0
#     if [ -n "$inline_datumhash" ]; then
#         inline_datumhash_bytes=$(echo -n "$inline_datumhash" | wc -c)
#         inline_datumhash_bytes=$((inline_datumhash_bytes / 2))
#     fi

#     # Remove inlineDatum and inlineDatumhash fields for remaining size calculation
#     reference_without_bytes=$(echo "$query" | jq -r 'del(.inlineDatum, .inlineDatumhash)')

#     # Calculate the size of the rest of the JSON structure
#     reference_size=$(echo "$reference_without_bytes" | jq -c '.' | wc -c)

#     # Calculate the total size in bytes for this TxOutRef
#     txoutref_size=$((reference_size + inline_datum_bytes + inline_datumhash_bytes))

#     echo "Estimated size for ref-input $oref: $txoutref_size bytes."

#     # Add the size of this TxOutRef to the total size
#     total_references_size=$((total_references_size + txoutref_size))

# done

# #-------------------------- :outputs size: ---------------------------

# outputs_tx_out_refs=($(jq -r '.outputs[]' $assetspath/inputs.json))

# total_outputs_size=0

# for oref in "${outputs_tx_out_refs[@]}"; do
#     output_size=$(echo -n "$oref" | wc -c)
    
#     # Add the size of the current oref to the total size
#     total_outputs_size=$((total_outputs_size + output_size))

# done

# echo "Estimated size for all outputs: $total_outputs_size bytes."

# #-------------------------- :validity size: ---------------------------

# validRangeSlots=$(jq -r '."valid-range-slots"' $assetspath/inputs.json)
# currentSlot=$(cardano-cli conway query tip --testnet-magic 4 | jq -r '.slot')
# uptoSlot=$(($currentSlot + $validRangeSlots))

# validity_size=$(echo -n "$uptoSlot" | wc -c)
	
# echo "Estimated size for validity interval: $validity_size bytes."

#------------------ :symbolicVerifier exec units: -------------------

total_size=$((total_inputs_size + total_references_size + total_outputs_size + validity_size))

echo ""
echo "Estimated total size of Plonk's input to hash: $total_size bytes."

# Linear model for memory units (b1 + m1 * size)
b1=1929741
m1=0.25
mem_symbolicVerifier=$(awk "BEGIN {printf \"%.0f\", $b1 + $m1 * $total_size}")

# Linear model for step units (b2 + m2 * size)
b2=4083915494
m2=27734.6
steps_symbolicVerifier=$(awk "BEGIN {printf \"%.0f\", $b2 + $m2 * $total_size}")

#-------------------------- :calculate minFee: ---------------------------

in1=$(jq -r '.inputs[0]' $assetspath/inputs.json)    # Bob
in2=$(jq -r '.inputs[1]' $assetspath/inputs.json)    # example: alwaysSucceds (with unit datum)
in3=$(jq -r '.inputs[2]' $assetspath/inputs.json)    # symbolicVerifier

ref1=$(jq -r '.inputs[2]' $assetspath/inputs.json)   # example: alwaysSucceeds (with plonk datum)

out1=$(jq -r '.outputs[0]' $assetspath/inputs.json)  # Payment to Alice
out2=$(jq -r '.outputs[1]' $assetspath/inputs.json)  # Change to Bob

collateral=$in1

# Building draft transaction.
cardano-cli conway transaction build-raw \
  --tx-in $in1 \
  --tx-in $in2 \
  --tx-in-script-file $assetspath/alwaysSucceeds.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-cbor-file $unitRedeemer \
  --tx-in-execution-units "$alwaysSucceedsExecUnits" \
  --tx-in $in3 \
  --spending-tx-in-reference $symbolicVerifier \
  --spending-plutus-script-v3 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-cbor-file $proofRedeemer \
  --spending-reference-tx-in-execution-units "($mem_symbolicVerifier, $steps_symbolicVerifier)" \
  --read-only-tx-in-reference $ref1 \
  --tx-in-collateral $collateral \
  --tx-out "$out1" \
  --tx-out "$out2" \
  --invalid-hereafter $uptoSlot \
  --fee 0 \
  --protocol-params-file $assetspath/protocol.json \
  --out-file $keypath/tx.draft

# Size of 'symbolicVerifier' in bytes.
symbolicVerifier_bytes=$(($(echo -n "$symbolicVerifier_cbor_hex" | wc -c) / 2))

# Minimum Fee calculation.
minFee=$(cardano-cli conway transaction calculate-min-fee \
  --tx-body-file $keypath/tx.draft \
  --protocol-params-file $assetspath/protocol.json \
  --witness-count 1 \
  --reference-script-size $symbolicVerifier_bytes | sed 's/ .*//')

echo ""
echo "Estimated minFee: $minFee lovelace."
echo ""
