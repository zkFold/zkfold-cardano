#! /bin/bash

set -e
set -u
set -o pipefail

keypath=./keys

cabal run zkfold-cli:asterizm -- relayer \
  --signing-key-file $keypath/bob.skey \
  --beneficiary-address $(cat $keypath/bob.addr)
