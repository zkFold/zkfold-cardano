#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

sanchomagic=4
keypath=./plonkVerifierToken/keys
privpath=./plonkVerifierToken/priv

echo "Create someone, zkfold-setup, alice and bob."

if [ -d "./plonkVerifierToken" ]; then
    mkdir -p $keypath
    mkdir -p $privpath
else
    echo "Please run script from directory 'e2e-test'."
    exit 1
fi

printf "$sanchomagic" > $privpath/testnet.flag
magic=$sanchomagic

#---------------------------------- :someone: ----------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/someone.vkey \
  --signing-key-file $keypath/someone.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/someone.vkey \
  --out-file $keypath/someone.addr \
  --testnet-magic $magic

#----------------------------------- :charles: -----------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/charles.vkey \
  --signing-key-file $keypath/charles.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/charles.vkey \
  --out-file $keypath/charles.addr \
  --testnet-magic $magic

#----------------------------------- :alice: -----------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/alice.vkey \
  --signing-key-file $keypath/alice.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/alice.vkey \
  --out-file $keypath/alice.addr \
  --testnet-magic $magic

#------------------------------------ :bob: ------------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/bob.vkey \
  --signing-key-file $keypath/bob.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/bob.vkey \
  --out-file $keypath/bob.addr \
  --testnet-magic $magic

#-------------------------------- :zkfold-main: --------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/zkfold-main.vkey \
  --signing-key-file $keypath/zkfold-main.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/zkfold-main.vkey \
  --out-file $keypath/zkfold-main.addr \
  --testnet-magic $magic

#-------------------------------------------------------------------------------

echo "Put some funds in the address(someone.addr): $(cat $keypath/someone.addr)"
# echo "Put some funds in the address(charles.addr): $(cat $keypath/charles.addr)"
# echo "Put some funds in the address(alice.addr): $(cat $keypath/alice.addr)"
# echo "Put some funds in the address(bob.addr): $(cat $keypath/bob.addr)"
