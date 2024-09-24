# Testing empty-circuit


## Summary

We test [symbolicVerifier](../../src/ZkFold/Cardano/Scripts/SymbolicVerifier.hs) using a **proof** generated with an "empty circuit", which corresponds to a "tautology function" (always true).  See [EmptyCircuit.hs](../../src/ZkFold/Cardano/Benchs/EmptyCircuit.hs) .

We expected that using this proof, the plutus script compiled from `symbolicVerifier` would always succeed, independently of the input.

Testing results suggest that this is not the case.

## Execution instructions

Assuming that active directory is `bench-emptycircuit`:

Execute, in order:

- `./scripts/01-init-params.sh`
- `./scripts/02-init-script-addrs.sh`
- `./scripts/03-init-fund-scripts.sh`
- `./scripts/04-init-park-script.sh`

Then execute:

```shell
./scripts/estimate-costs.sh &> error.log
```
