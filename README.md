# zkFold: Cardano integration library
Integration of zkFold Symbolic smart contracts with the Cardano blockchain. The repository contains
- the Plutus script for the zkFold Symbolic verifier;
- the Plutus script for the Plonk proof verification token;
- the off-chain code for transaction building;
- benchmarks for the Plutus scripts;
- end-to-end tests for the zkFold Symbolic smart contracts.

## Building the project

This project can be built with Cabal 3.10.2.1 and GHC 9.6.3.

Crypotgraphic depencencies needed for building Haskell packages:

* [`libsodium`](https://github.com/jedisct1/libsodium)
* [`libsecp256k1`](https://github.com/bitcoin-core/secp256k1)
* [`libblst`](https://github.com/supranational/blst)

```
$ cabal update
$ cabal build
```

## Running benchmarks and end-to-end tests

### Importing circuits from `zkfold-base`

TODO

### Test

`cabal run test` to check the implementations against the test data.

### Benchmarks (in progress)

`cabal run bench-cpu-mem` to calculate the cpu/mem units of the tests in the plutus-benchmark package.

`cabal run bench-analyze-uplc` to compile the fast implementation to UPLC (`.flat`) for the further processing.

### Setting up a local testnet

Create a local testnet with `local-testnet/scripts/babbage/mkfiles.sh`.

Run with `local-testnet/example/run/all.sh`.

### End-to-end tests (in progress)

We have two types of Plutus scripts that facilitate zero-knowledge smart contracts:

1) **_Plonk verifier_** mints a token if the statement expressed as an arithmetic circuit is correct. The token name plays the role of the public input to the ZKP protocol.
2) **_Symbolic verifier_** validates a transaction that withdraws ada rewards. The transaction data is hashed and supplied as a public input to the ZKP protocol.

General workflow:
- Create a trusted setup for the Plonk protocol;
- Compile the zero-knowledge smart contract into an arithmetic circuit;
- Deploy a local testnet or use sancho.network testnet;
- Fund a public key address on the testnet;
- Generate a proof from a witness and create the redeemer data for the Plutus scripts;
- Construct a transaction with the Plutus scripts using `cardano-cli`;
- Submit transactions using `cardano-cli`.
