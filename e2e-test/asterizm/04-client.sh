#! /bin/bash

set -e
set -u
set -o pipefail

keypath=./keys

cabal run zkfold-cli:asterizm -- client \
  --signing-key-file $keypath/alice.skey \
  --beneficiary-address $(cat $keypath/alice.addr)

