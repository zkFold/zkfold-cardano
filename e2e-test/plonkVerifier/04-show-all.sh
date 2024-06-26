#! /bin/bash

keypath=./keys

echo ""
echo "alice address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"
echo ""
echo "bob address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/bob.addr) --testnet-magic 4)"
echo ""
echo "tokenVerifier address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/tokenVerifier.addr) --testnet-magic 4)"
echo ""
echo "zkfold-main address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/zkfold-main.addr) --testnet-magic 4)"
echo ""
