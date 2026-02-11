#! /bin/bash

set -e
set -u
set -o pipefail

keypath=./keys

# The init command now only computes the relayer currency symbols
# and writes them to asterizm-setup.json - no on-chain transaction needed.

cabal run zkfold-cli:asterizm -- init \
  --client-vkey-file $keypath/client.vkey \
  --relayer-vkey-file $keypath/relayer.vkey
