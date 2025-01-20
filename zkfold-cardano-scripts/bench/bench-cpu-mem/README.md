# Benchmarks for different Plutus scripts

`cabal run bench-cpu-mem`        to calculate the cpu/mem units of the tests in the plutus-benchmark package.

This repository will follow [plutus-plonk-poc](https://github.com/perturbing/plutus-plonk-poc).

```bash
Run 'plonkVerifier'

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    5167  (31.5%)      5475398242  (54.8%)         1329610   (9.5%)
```

```bash
Run 'plonkVerifierToken'

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    5614  (34.3%)      5478141917  (54.8%)         1325175   (9.5%)
```

```bash
Run 'plonkVerifierTx'

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    5318  (32.5%)      5485836172  (54.9%)         1324775   (9.5%)
```