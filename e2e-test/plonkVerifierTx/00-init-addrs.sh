#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys

echo "Create someone, zkfold-setup, alice and bob."

mkdir -p $keypath

#----------------------------------- :alice: -----------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/alice.vkey \
  --signing-key-file $keypath/alice.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/alice.vkey \
  --out-file $keypath/alice.addr \
  --testnet-magic 4

#------------------------------------ :bob: ------------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/bob.vkey \
  --signing-key-file $keypath/bob.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/bob.vkey \
  --out-file $keypath/bob.addr \
  --testnet-magic 4

#---------------------------------- :someone: ----------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/someone.vkey \
  --signing-key-file $keypath/someone.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/someone.vkey \
  --out-file $keypath/someone.addr \
  --testnet-magic 4

#-------------------------------- :zkfold-main: --------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/zkfold-main.vkey \
  --signing-key-file $keypath/zkfold-main.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/zkfold-main.vkey \
  --out-file $keypath/zkfold-main.addr \
  --testnet-magic 4

#-------------------------------------------------------------------------------

echo "Put some funds in the address(someone.addr): $(cat $keypath/someone.addr)"
echo "Put some funds in the address(alice.addr): $(cat $keypath/alice.addr)"
echo "Put some funds in the address(bob.addr): $(cat $keypath/bob.addr)"
