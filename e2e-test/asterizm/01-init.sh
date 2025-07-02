#! /bin/bash

set -e
set -u
set -o pipefail

keypath=./keys

# Substitute this sample UTxO reference with your choice:
outref=4e16927f137a748a1ac7f19c26166695cb8a0031f1fc16b098d793f357aeb13f#0

cabal run zkfold-cli:asterizm -- init \
  --signing-key-file $keypath/someone.skey \
  --tx-oref $outref \
  --registry-address $(cat $keypath/asterizm.addr) \
  --client-vkey-file $keypath/alice.vkey \
  --relayer-vkey-file $keypath/bob.vkey \
  --relayer-vkey-file $keypath/charlie.vkey \
  --relayer-vkey-file $keypath/dylan.vkey
