# Analyze uplc

`cabal run bench-analyze-uplc` to compile the fast implementation to UPLC (`.flat`) for further processing.

This repository will follow [plutus-plonk-poc](https://github.com/perturbing/plutus-plonk-poc).

You can analyze your script uplc.

You can get uplc, traceToStacks from plutus-core
```
cabal install plutus-core
```
```
uplc evaluate -t -i asserts/plonkVerifyScript.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs
uplc evaluate -t -i asserts/plonkVerifierScript.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs
uplc evaluate -t -i asserts/symbolicVerifierScript.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs
```

You can get flamegraph.pl from [github](https://github.com/brendangregg/FlameGraph), or `nix shell nixpkgs#flamegraph`.

to log the CPU/MEM consumption
```
cat logs | traceToStacks | flamegraph.pl > cpu.svg
cat logs | traceToStacks --column 2 | flamegraph.pl > mem.svg
```
for more info see https://hydra.family/head-protocol/benchmarks/profiling/
and https://plutus.readthedocs.io/en/latest/howtos/profiling-scripts.html
