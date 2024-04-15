# Benchmark of plonk

`cabal run bench-cpu-mem`        to calculate the cpu/mem units of the tests in the plutus-benchmark package.

This repository will follow [plutus-plonk-poc](https://github.com/perturbing/plutus-plonk-poc).

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