# Benchmarks for different Plutus scripts

`cabal run bench-cpu-mem`        to calculate the cpu/mem units of the tests in the plutus-benchmark package.

This repository will follow [plutus-plonk-poc](https://github.com/perturbing/plutus-plonk-poc).

```bash
Run 'verifyPlonk'

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    4040  (24.7%)      3804030393  (38.0%)          800065   (5.7%) 
```

```bash
Run 'plonkVerifier'

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -   11680  (71.3%)      3900648512  (39.0%)         1195522   (8.5%)
```

```bash
Run 'symbolicVerifier'

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    4736  (28.9%)         4968100   (0.0%)           21700   (0.2%) 
```