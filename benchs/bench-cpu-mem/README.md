# Benchmarks for different Plutus scripts

`cabal run bench-cpu-mem`        to calculate the cpu/mem units of the tests in the plutus-benchmark package.

This repository will follow [plutus-plonk-poc](https://github.com/perturbing/plutus-plonk-poc).

```bash
Run plonk verify

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    3197  (19.5%)      3810767302  (38.1%)          845905   (6.0%)
```

```bash
Run plonk verifier

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    4036  (24.6%)      3816934967  (38.2%)          895030   (6.4%) 
```

```bash
Run symbolic plonk verifier

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    4736  (28.9%)         4968100   (0.0%)           21700   (0.2%) 
```