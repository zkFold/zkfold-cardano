# Rollup scripts


These scripts implement a prototype *rollup loop*.

## Instructions

Assuming that your active directory is `e2e-test/rollup`, execute the following.

0. Create Alice's wallet and fund it:
```shell
./00-sancho-init-addrs.sh
```
1. Initialize rollup process:
```shell
./01-rollup-init-transaction.sh
```
2. Start rollup update loop:
```shell
./02-rollup-update-loop.sh
```
You will enter a loop of recurrent rollup updates.  To **stop**, execute (from another terminal)
```shell
printf "false" > ./keys/rollup-loop.flag
```
