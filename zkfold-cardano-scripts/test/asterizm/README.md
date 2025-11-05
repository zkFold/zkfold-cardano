# Testing *buildCrosschainHash*

Test checks that hashes produced by [**Plutus**](../../src/ZkFold/Cardano/UPLC/Asterizm.hs#L40) and [**Solidity**](https://github.com/Asterizm-Protocol/asterizm-contracts-evm/blob/master/contracts/libs/AsterizmHashLib.sol) versions of `buildCrosschainHash` agree.

## Run test

```shell
cabal test buildCrosschainHash-test
```
