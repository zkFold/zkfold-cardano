# Cli scripts

## Docs

We have a description of transactions in `src-docs`.

## Scripts

Here we will describe bash scripts for executing transactions on the sancho.network testnet.

### Plonk

Run the `00-init-addrs.sh` to initialize the system, you also need to transfer money to addresses.

To start a transaction you need to run the following 5 scripts.

- `plonk/01-init-transaction.sh` for publishing Plutus scripts on the blockchain.
- `plonk/02-transfer-transaction.sh` to create rewards when burning tokens.
- `plonk/03-minting-transaction.sh` creation and sending of tokens.
- `plonk/04-burning-transaction.sh` burning and receiving rewards.

You can also see all wallet at `05-show-all.sh`.

### Symbolic
