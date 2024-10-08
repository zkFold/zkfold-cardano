# Rollup scripts


These scripts implement a prototype rollup process.

## Instructions

Assuming that your active directory is `e2e-test/rollup`, execute the following.

0. (SKIP since Alice's keys already present and funded.)  If necessary, create Alice's wallet and fund it:
```shell
./00-sancho-init-addrs.sh
```
1. Initialize rollup process:
```shell
./01-rollup-init-transaction.sh
```
2. Recurrent rollup update:
```shell
./02-rollup-update-transaction.sh
```
Execute step 2 recurrently (again & again) to simulate each rollup update.
