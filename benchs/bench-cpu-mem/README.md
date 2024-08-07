# Benchmarks for different Plutus scripts

`cabal run bench-cpu-mem`        to calculate the cpu/mem units of the tests in the plutus-benchmark package.

This repository will follow [plutus-plonk-poc](https://github.com/perturbing/plutus-plonk-poc).

```bash
Run plonk verify

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    4132  (25.2%)      3396538371  (34.0%)          189481   (1.4%) 
```

```bash
Run plonk verifier

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    4249  (25.9%)         3864100   (0.0%)           16900   (0.1%) 
```

```bash
Run symbolic plonk verifier

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    4736  (28.9%)         4968100   (0.0%)           21700   (0.2%) 
```