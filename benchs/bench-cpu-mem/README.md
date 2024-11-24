# Benchmarks for different Plutus scripts

`cabal run bench-cpu-mem`        to calculate the cpu/mem units of the tests in the plutus-benchmark package.

This repository will follow [plutus-plonk-poc](https://github.com/perturbing/plutus-plonk-poc).

```bash
Run 'plonkVerifier'

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    3840  (23.4%)      3778144985  (37.8%)          738280   (5.3%) 
```

```bash
Run 'plonkVerifierToken'

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -   11796  (72.0%)      3882216908  (38.8%)         1173825   (8.4%) 
```

```bash
Run 'plonkVerifierTx'

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    3992  (24.4%)      3786792153  (37.9%)          720971   (5.1%) 
```