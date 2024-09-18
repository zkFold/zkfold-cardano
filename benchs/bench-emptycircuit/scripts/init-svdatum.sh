#! /bin/bash

set -e
set -u
set -o pipefail

keypath="./scripts/keys"
assetspath="../../assets"

policyid=$(cardano-cli conway transaction policyid --script-file "$assetspath/symbolicVerifier.plutus")

echo "policyid: $policyid"

cabal run symbolic-transfer-transaction -- $policyid

