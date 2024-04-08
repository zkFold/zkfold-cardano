# zkFold: Cardano integration library
An integration of zkFold Symbolic smart contracts with the Cardano blockchain. The repository contains
- the Plutus script for the zkFold Symbolic verifier;
- the Plutus script for the Plonk proof verification token;
- the off-chain code for transaction building.

## Building the project

This project can be built with Cabal 3.10.2.0 and GHC 9.6.3.

### With `nix`

With nix it is as easy as:

```
$ nix develop
...
$ cabal build all
```

### Without `nix`

Crypotgraphic depencencies needed for building Haskell packages:

* [`libsodium`](https://github.com/jedisct1/libsodium)
* [`libsecp256k1`](https://github.com/bitcoin-core/secp256k1)
* [`libblst`](https://github.com/supranational/blst)

We provide packaged versions for common Operating Systems for all of the above
dependencies: [Download](https://github.com/input-output-hk/iohk-nix/releases/latest)
