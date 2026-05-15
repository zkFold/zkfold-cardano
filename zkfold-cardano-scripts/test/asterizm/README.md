# Testing Asterizm hash functions

Test checks `buildCrosschainHash` against the [**Solidity**](https://github.com/Asterizm-Protocol/asterizm-contracts-evm/blob/master/contracts/libs/AsterizmHashLib.sol) implementation and checks `buildHash` against plain SHA-256 vectors.

## Run test

```shell
cabal test buildCrosschainHash-test
```
