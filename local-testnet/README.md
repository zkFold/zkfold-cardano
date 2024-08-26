# Cli scripts

## Docs

We have a description of transactions in `../e2e-test/src-docs`.

## Scripts

Here we will describe bash scripts for executing transactions on the local testnet.

In following steps, it is assumed that your	active directory is `local-testnet`.

### Setting up a local testnet

Create a local testnet with `./scripts/babbage/mkfiles.sh`.

Run with `./example/run/all.sh`.  (To stop the testnet, type ^C twice.)

Open another shell (where you are going to run the transactions) and execute
```shell
export CARDANO_NODE_SOCKET_PATH=<path-to-zkfold-cardano>/local-testnet/example/main.sock
```

After a few moments, execute
```shell
cardano-cli conway query tip --testnet-magic 42
```
If "syncProgress" is less than 100%, stop the testnet, `sudo rm -r ./example`, and restart the testnet.

### Plonk

Run `./scripts/plonk/00-init-system.sh` to initialize the system.

To perform the transactions, run the following 4 scripts.

- `./scripts/plonk/01-init-transaction.sh` to publish Plutus scripts on the blockchain.
- `./scripts/plonk/02-transfer-transaction.sh` to set up a reward for burning a token.
- `./scripts/plonk/03-minting-transaction.sh` to mint and send a token to the owner.
- `./scripts/plonk/04-burning-transaction.sh` to burn the token and receive the reward.

You can also see the state of all wallets with `./scripts/plonk/05-show-all.sh`.

### Symbolic
