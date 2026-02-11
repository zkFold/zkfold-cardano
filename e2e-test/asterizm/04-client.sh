#! /bin/bash

set -e
set -u
set -o pipefail

keypath=./keys

cabal run zkfold-cli:asterizm -- client \
  --signing-key-file $keypath/client.skey \
  --beneficiary-address $(cat $keypath/client.addr)

