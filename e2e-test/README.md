# Cli scripts


## Docs

We have a description of transactions in `src-docs`.


## Scripts

Execution instructions for *Plonk*, *Rollup* and *Symbolic*.

### Plonk

You can choose to run the plonk verifier example on either **SanchoNet** or a **local** testnet.  The initial-setup depends on your choice.

In either case, make `e2e-test` your active directory.

#### Initial setup

##### SanchoNet

It is assumed that you have *cardano-node* running and have executed

```shell
export CARDANO_NODE_SOCKET_PATH=<path-to-node.socket>
```
on your working shell.

To initialize the system:

- Execute `./plonk/sancho-init-addrs.sh` to initialize needed wallets.  The address of *someone*'s wallet will be displayed.
- Fund *someone*'s wallet (typically using SanchoNet's public [Faucet](https://sancho.network/faucet)).
- Execute `./plonk/sancho-init-funding.sh` to fund the remaining wallets.  (Alternatively, manually transfer some Ada to all generated wallets, but make sure *someone*'s wallet gets at least two UTxO's with funds.)

##### Local testnet (initialization)

a. Create a local testnet with `./local-testnet/scripts/babbage/mkfiles.sh`.

b. Start the testnet with `./local-testnet/example/run/all.sh`.  (To stop the testnet, press ^C twice.)

Open another shell (where you are going to run the transactions, making `e2e-test` the active directory), and execute
```shell
export CARDANO_NODE_SOCKET_PATH=<path-to-e2e-test>/local-testnet/example/main.sock
```

After a few moments, execute
```shell
cardano-cli conway query tip --testnet-magic 42
```
If "syncProgress" is less than 100%, stop the testnet, `sudo rm -r ./local-testnet/example`, recreate the local testnet and restart the testnet (steps a. and b. above).

This concludes initialization of the local testnet.

To initialize the plonk system, run `./plonk/local-init-system.sh`.

#### Plonk Transactions

To perform the transactions (on either SanchoNet or local testnet), execute the following 4 scripts.

- `./plonk/01-init-transaction.sh` to publish Plutus scripts on the blockchain.
- `./plonk/02-transfer-transaction.sh` to set up a reward for burning a token.
- `./plonk/03-minting-transaction.sh` to mint and send a token to the owner.
- `./plonk/04-burning-transaction.sh` to burn the token and receive the reward.

You can also see the state of all wallets with `./plonk/05-show-all.sh`.

### Rollup

Rollup scripts implement a prototype *rollup loop*.  Currently rollup transactions process batches of two rollup updates, which are submitted as two parallel transactions.

For concreteness, in this prototype the "rollup update" (which is a list) is constructed by appending the "state" (which is a field element) associated to the previous update.  This can be easily generalized.

To execute the following steps, please make `e2e-test` your active directory.

#### Initialization (SanchoNet or local testnet)

<u>On SanchoNet</u>:

a. Create Alice's wallet and fund it:
```shell
./rollup/sancho-iniA.sh
```
b. After funding wallet, split single UTxO into two:
```shell
./rollup/sancho-iniB.sh
```

<u>On local testnet</u>:

(To initialize local testnet, follow instructions in section *Plonk* above.)

Create Alice's wallet, fund it and split single UTxO into two:
```shell
./rollup/local-ini.sh
```

#### Rollup loop

After performing the above initialization (on either SanchoNet or local testnet),

1. Initialize rollup process:
```shell
./rollup/01-rollup-init-transaction.sh
```
2. Start rollup update loop [currently only one rollup transaction]:
```shell
./rollup/02-rollup-update-transaction.sh
```
You will enter a loop of recurrent rollup updates.  To **stop**, execute (from another terminal)
```shell
printf "false" > ./rollup/keys/rollup-loop.flag
```
To resume rollup update loop, simply execute step 2 again.  To reset and start all over again from step 1, execute `./rollup/reset.sh`.

*Note:*  if working on local testnet, keep in mind that it sometimes behaves erratically, like failing to submit Txs even if message "Transaction successfully submitted" is displayed.


### Symbolic

(In progress.)
