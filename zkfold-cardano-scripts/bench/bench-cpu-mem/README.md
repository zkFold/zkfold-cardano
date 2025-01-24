# Benchmarks for different Plutus scripts

`cabal run bench-cpu-mem`        to calculate the cpu/mem units of the tests in the plutus-benchmark package.

This repository will follow [plutus-plonk-poc](https://github.com/perturbing/plutus-plonk-poc).

```bash
Run 'plonkVerifier'

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    5365  (32.7%)      5476475230  (54.8%)         1334302   (9.5%) 




Run 'plonkVerifierToken'

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    5614  (34.3%)      5478141917  (54.8%)         1325175   (9.5%) 




Run 'plonkVerifierTx'

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    9343  (57.0%)      5510703069  (55.1%)         1428243  (10.2%) 
```