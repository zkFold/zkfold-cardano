# Cli scripts


## Docs

We have a description of transactions in `src-docs`.


## Scripts

### Plonk

You can choose to run the plonk verifier example on either **SanchoNet** or a **local** testnet.  The initial-setup depends on your choice.

In either case, make `e2e-test` your active directory.

### Initial setup

#### SanchoNet

It is assumed that you have *cardano-node* running and have executed

```shell
export CARDANO_NODE_SOCKET_PATH=<path-to-node.socket>
```
on your working shell.

To initialize the system:

- Execute `./plonk/sancho-init-addrs.sh` to initialize needed wallets.  The address of *someone*'s wallet will be displayed.
- Fund *someone*'s wallet (typically using SanchoNet's public [Faucet](https://sancho.network/faucet)).
- Execute `./plonk/sancho-init-funding.sh` to fund the remaining wallets.  (Alternatively, manually transfer some Ada to all generated wallets, but make sure *someone*'s wallet gets at least two UTxO's with funds.)

#### Local testnet

Create a local testnet with `./local-testnet/scripts/babbage/mkfiles.sh`.

Start the testnet with `./local-testnet/example/run/all.sh`.  (To stop the testnet, press ^C twice.)

Open another shell (where you are going to run the transactions, making `e2e-test` the active directory), and execute
```shell
export CARDANO_NODE_SOCKET_PATH=<path-to-e2e-test>/local-testnet/example/main.sock
```

After a few moments, execute
```shell
cardano-cli conway query tip --testnet-magic 42
```
If "syncProgress" is less than 100%, stop the testnet, `sudo rm -r ./local-testnet/example`, `./local-testnet/scripts/babbage/mkfiles.sh`, and restart the testnet.

Run `./plonk/local-init-system.sh` to initialize the system.

### Transactions

To perform the transactions (on either SanchoNet or local testnet), execute the following 4 scripts.

- `./plonk/01-init-transaction.sh` to publish Plutus scripts on the blockchain.
- `./plonk/02-transfer-transaction.sh` to set up a reward for burning a token.
- `./plonk/03-minting-transaction.sh` to mint and send a token to the owner.
- `./plonk/04-burning-transaction.sh` to burn the token and receive the reward.

You can also see the state of all wallets with `./plonk/05-show-all.sh`.


### Symbolic
