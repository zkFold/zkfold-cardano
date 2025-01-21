# Cli scripts


## Docs

We have a description of transactions in `docs`.


## Scripts

Execution instructions for *PlonkVerifierToken*, *Rollup* and *PlonkVerifierTx*.

### PlonkVerifierToken

You can choose to run the `plonkVerifierToken` on either **SanchoNet** or a **local** testnet.  The initial-setup depends on your choice.

In either case, make `e2e-test` your active directory.

#### Initial setup

##### SanchoNet

It is assumed that you have *cardano-node* running and have executed

```shell
export CARDANO_NODE_SOCKET_PATH=<path-to-node.socket>
```
on your working shell.

To initialize the system:

- Execute `./plonkVerifierToken/sancho-init-addrs.sh` to initialize needed wallets.  The address of *someone*'s wallet will be displayed.
- Fund *someone*'s wallet (typically using SanchoNet's public [Faucet](https://sancho.network/faucet)).
- Execute `./plonkVerifierToken/sancho-init-funding.sh` to fund the remaining wallets.  (Alternatively, manually transfer some Ada to all generated wallets, but make sure *someone*'s wallet gets at least two UTxO's with funds.)

##### Local testnet (initialization)

a. Create a local testnet with `./local-testnet/e2e-test/babbage/mkfiles.sh`.

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

To initialize the plonk system, run `./plonkVerifierToken/local-init-system.sh`.

#### PlonkVerifierToken Transactions

To perform the transactions (on either SanchoNet or local testnet), execute the following 4 scripts.

- `./plonkVerifierToken/01-init-transaction.sh` to publish Plutus scripts on the blockchain.
- `./plonkVerifierToken/02-transfer-transaction.sh` to set up a reward for burning a token.
- `./plonkVerifierToken/03-minting-transaction.sh` to mint and send a token to the owner.
- `./plonkVerifierToken/04-burning-transaction.sh` to burn the token and receive the reward.

You can also see the state of all wallets with `./plonkVerifierToken/05-show-all.sh`.

### Rollup

Rollup scripts implement a prototype *rollup loop*.  Currently rollup transactions process batches of three update data tokens (minted in "parallel" transactions).

Every five rollup transactions, used data tokens are burned.

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

(To initialize local testnet, follow instructions in section *PlonkVerifierToken* above.)

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
2. Start rollup update loop:
```shell
./rollup/02-rollup-update-loop.sh
```
You will enter a loop of recurrent rollup updates.  To **stop**, execute (from another terminal)
```shell
printf "false" > ./rollup/keys/rollup-loop.flag
```
To resume rollup update loop, simply execute step 2 again.  To reset and start all over again from step 1, execute `./rollup/reset.sh`.

*Note:*  if working on local testnet, keep in mind that it sometimes behaves erratically, like failing to submit Txs even if message "Transaction successfully submitted" is displayed.


### PlonkVerifierTx balancing

Instructions:  Create local testnet (follow instruction in section *PlonkVerifierToken* above).  Then, from directory `e2e-test`, execute the following shell commands.

0. Create Alice's wallet and fund it:
```shell
./plonkVerifierTx-balancing/local-ini.sh
```

1. Initialization and funding of PlonkVerifierTx script address:
```shell
./plonkVerifierTx-balancing/01-balancing-init-transaction.sh
```

2. Get exec units and estimated fee for retrieval transaction:
```shell
./plonkVerifierTx-balancing/02-balancing-parse-input.sh
```

### PlonkVerifierTx

(In progress.)
