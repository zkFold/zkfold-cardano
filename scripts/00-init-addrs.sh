#! /bin/bash

keypath=./keys

echo "Create alice, bob and someone."

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

#-------------------------------------------------------------------------------

echo "Put some funds in the address(alice.addr): $(cat $keypath/alice.addr)"
echo "Put some funds in the address(someone.addr): $(cat $keypath/someone.addr)"
