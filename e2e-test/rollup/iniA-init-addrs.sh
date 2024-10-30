#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
mN=42

echo "Create Alice..."

mkdir -p $keypath

#----------------------------------- :alice: -----------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/alice.vkey \
  --signing-key-file $keypath/alice.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/alice.vkey \
  --out-file $keypath/alice.addr \
  --testnet-magic $mN

#-------------------------------------------------------------------------------

echo "Done.  Put some funds in Alice's address: $(cat $keypath/alice.addr)"
