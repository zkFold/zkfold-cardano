#! /bin/bash

set -e
set -u
set -o pipefail

keypath=./keys

# Read message from file (saved in 02-message.sh)
message=$(cat ./assets/message.hex)

cabal run zkfold-cli:asterizm -- client \
  --signing-key-file $keypath/client.skey \
  --beneficiary-address $(cat $keypath/client.addr) \
  --message "$message" \
  --incoming

