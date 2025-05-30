# Rollups benchmarks


Here we benchmark `rollup` script defined in `../../src/ZkFold/Cardano/UPLC/Rollup.hs`.

## Exec units as a function of *update length*

Instructions for running the benchmarks and summary of results.

### Running the benchmarks

Execute:

```shell
cabal run bench-rollup
```

### Summary of results

![data plots](./data-analysis/rollupBench.png)

Therefore, to be within budget, *update length* must be 552 or less.

For details, see [data-analysis.pdf](./data-analysis/data-analysis.pdf).
