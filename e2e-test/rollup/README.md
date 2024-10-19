# Rollup scripts


Scripts implementing a prototype *rollup loop*.  Currently rollup transactions process batches of two rollup updates, which are submitted as two parallel transactions.

For concreteness, in this prototype the "rollup update" (which is a list) is constructed by appending the "state" (which is a field element) associated to the previous update.  This can be easily generalized.

## Instructions

Assuming that your active directory is `e2e-test/rollup`, execute the following.

### Initialize Alice's wallet

a. Create Alice's wallet and fund it:
```shell
./iniA-init-addrs.sh
```
b. After funding wallet, split single UTxO into two:
```shell
./iniB-split-alice.sh
```

### Rollup loop

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
To resume rollup update loop, simply execute step 2 again.  To reset and start all over again from *step 1*, execute `./reset.sh`.

### Debug

It is expected that rollup loop will continue indefinitely (until max exec-units are reached). If for some reason second Tx fails to submit, you can try `./debug-second-tx-A.sh`, and if this goes through, cleanup with `./debug-second-tx-B.sh`.  Then resume with step 2 above.