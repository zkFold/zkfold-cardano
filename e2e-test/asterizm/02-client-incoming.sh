#! /bin/bash

set -e
set -u
set -o pipefail

configpath=./assets/config.json
keypath=./keys

# Read the incoming message (same message the relayer attested)
message=$(cat ./assets/message-incoming.hex)

echo "Submitting incoming message to client..."

cabal run zkfold-cli:asterizm -- client receive \
  --core-config-file $configpath \
  --signing-key-file $keypath/client.skey \
  --client-vkey-file $keypath/client.vkey \
  --relayer-vkey-file $keypath/relayer.vkey \
  --beneficiary-address $(cat $keypath/client.addr) \
  --message "$message"
