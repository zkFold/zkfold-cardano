# zkFold: Cardano integration library
Integration of zkFold Symbolic smart contracts with the Cardano blockchain. The repository contains
- current zk protocols in `zkfold-cardano`;
- the Plutus scripts in `zkfold-contracts`;
- off-chain code for constructing and sending transactions in `zkfold-backends`;
- examples of zk circuits in `zkfold-example`;
- end-to-end tests for smart contracts in `scripts`;
- current smart contract documentation in `docs`.

## Building the project

This project can be built with Cabal 3.10.3.0 and GHC 9.6.6.

Crypotgraphic depencencies needed for building Haskell packages:

* [`libsodium`](https://github.com/jedisct1/libsodium)
* [`libsecp256k1`](https://github.com/bitcoin-core/secp256k1)
* [`libblst`](https://github.com/supranational/blst)

```
$ cabal update
$ cabal build
```

