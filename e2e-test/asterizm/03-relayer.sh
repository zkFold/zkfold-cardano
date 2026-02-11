#! /bin/bash

set -e
set -u
set -o pipefail

keypath=./keys

# Read message hash from file (computed in 02-message.sh)
messageHash=$(cat ./assets/message-hash.hex)

cabal run zkfold-cli:asterizm -- relayer \
  --signing-key-file $keypath/relayer.skey \
  --beneficiary-address $(cat $keypath/relayer.addr) \
  --message-hash "$messageHash"
