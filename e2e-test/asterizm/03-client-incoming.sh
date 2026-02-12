#! /bin/bash

set -e
set -u
set -o pipefail

export CORE_CONFIG_PATH=./assets/config.json
keypath=./keys

# Read the incoming message (same message the relayer attested)
message=$(cat ./assets/message-incoming.hex)

echo "Submitting incoming message to client..."

cabal run zkfold-cli:asterizm -- client \
  --signing-key-file $keypath/client.skey \
  --beneficiary-address $(cat $keypath/client.addr) \
  --message "$message" \
  --incoming
