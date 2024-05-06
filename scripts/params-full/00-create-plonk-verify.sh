#! /bin/bash -i

LOVELACE=1000000

keypath=../keys

echo "Put some funds in the address: $(cat $keypath/plonk-verify-input.addr)"

# input sign
cardano-cli conway address key-gen \
  --verification-key-file $keypath/plonk-verify-input.vkey \
  --signing-key-file $keypath/plonk-verify-input.skey

# input address
cardano-cli conway address build \
  --payment-verification-key-file $keypath/plonk-verify-input.vkey \
  --out-file $keypath/plonk-verify-input.addr \
  --testnet-magic 4
