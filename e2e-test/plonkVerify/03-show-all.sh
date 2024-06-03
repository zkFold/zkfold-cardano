#! /bin/bash

keypath=./keys

echo ""
echo "alice address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"
echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""
echo "plonkVerify address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/plonkVerify.addr) --testnet-magic 4)"
echo ""
