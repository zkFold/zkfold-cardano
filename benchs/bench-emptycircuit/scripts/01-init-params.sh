#! /bin/bash

set -e
set -u
set -o pipefail

keypath="./scripts/keys"
assetspath="../../assets"

cardano-cli conway query protocol-parameters \
  --testnet-magic 4 \
  --out-file $assetspath/protocol.json

