# Cli scripts


## Docs

We have a description of transactions in `src-docs`.


## Scripts

### Plonk

We have scripts for executing transactions on both **SanchoNet** and **local** testnets.

#### SanchoNet

Make `e2e-test` your active directory.  It is assumed that you have *cardano-node* running and have executed

```shell
export CARDANO_NODE_SOCKET_PATH=<path-to-node.socket>
```
on your working shell.

To initialize the system:

- Execute `./plonk/00a-init-addrs.sh` to initialize needed wallets.  The address of *someone*'s wallet will be displayed.
- Fund *someone*'s wallet (typically using SanchoNet's public [Faucet](https://sancho.network/faucet)).
- Execute `./plonk/00b-init-funding.sh` to fund the remaining wallets.  (Alternatively, manually transfer some Ada to all generated wallets, but make sure *someone*'s wallet gets at least two UTxO's with funds.)

To perform the transactions, execute the following 4 scripts.

- `./plonk/01-init-transaction.sh` to publish Plutus scripts on the blockchain.
- `./plonk/02-transfer-transaction.sh` to set up a reward for burning a token.
- `./plonk/03-minting-transaction.sh` to mint and send a token to the owner.
- `./plonk/04-burning-transaction.sh` to burn the token and receive the reward.

You can also see the state of all wallets with `./plonk/05-show-all.sh`.

#### Local testnet

Make `local-testnet` your active directory.

Create a local testnet with `./scripts/babbage/mkfiles.sh`.

Start the testnet with `./example/run/all.sh`.  (To stop the testnet, type ^C twice.)

Open another shell (where you are going to run the transactions) and execute
```shell
export CARDANO_NODE_SOCKET_PATH=<path-to-zkfold-cardano>/local-testnet/example/main.sock
```

After a few moments, execute
```shell
cardano-cli conway query tip --testnet-magic 42
```
If "syncProgress" is less than 100%, stop the testnet, `sudo rm -r ./example`, `./scripts/babbage/mkfiles.sh`, and restart the testnet.

Run `./scripts/plonk/00-init-system.sh` to initialize the system.

To perform the transactions, run the following 4 scripts.

- `./scripts/plonk/01-init-transaction.sh` to publish Plutus scripts on the blockchain.
- `./scripts/plonk/02-transfer-transaction.sh` to set up a reward for burning a token.
- `./scripts/plonk/03-minting-transaction.sh` to mint and send a token to the owner.
- `./scripts/plonk/04-burning-transaction.sh` to burn the token and receive the reward.

You can also see the state of all wallets with `./scripts/plonk/05-show-all.sh`.

### Symbolic

