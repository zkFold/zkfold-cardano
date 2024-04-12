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

## To run

1. `cabal run test`  to check the implementations against the test data.
2. `cabal run bench-uplc`        to compile the fast implementation to UPLC (`.flat`) for further processing.
3. `cabal run bench-cpu-mem`        to calculate the cpu/mem units of the tests in the plutus-benchmark package.

This repository will follow [this](https://github.com/perturbing/plutus-plonk-poc) repository.

## Analyze uplc
You can analyze your script uplc.

`uplc evaluate -t -i plonkVerifyScript.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs`
`uplc evaluate -t -i plonkVerifierScript.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs`
`uplc evaluate -t -i symbolicVerifierScript.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs`

to log the CPU/MEM consumption
```
cat logs | traceToStacks | flamegraph.pl > cpu.svg
cat logs | traceToStacks --column 2 | flamegraph.pl > mem.svg
```
for more info see https://hydra.family/head-protocol/benchmarks/profiling/
and https://plutus.readthedocs.io/en/latest/howtos/profiling-scripts.html

## Benchmark of plonk

```bash
Run plonk verify

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    4498  (27.5%)      3471674127  (34.7%)          332954   (2.4%) 
```

```bash
Run plonk verifier

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    4618  (28.2%)         5612100   (0.1%)           24500   (0.2%) 
```

```bash
Run symbolic plonk verifier

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    5116  (31.2%)         6992100   (0.1%)           30500   (0.2%)
```