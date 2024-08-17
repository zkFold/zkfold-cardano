# Cli scripts

## Docs

We have a description of transactions in `src-docs`.

## Scripts

Here we will describe bash scripts for executing transactions on the sancho.network testnet.

### Plonk

Run the `00-init-addrs.sh` to initialize the system.

Transfer some ada to all generated addresses.

To perform the transactions, run the following 4 scripts.

- `plonk/01-init-transaction.sh` to publish Plutus scripts on the blockchain.
- `plonk/02-transfer-transaction.sh` to set up a reward for burning a token.
- `plonk/03-minting-transaction.sh` to mint and send a token to the owner.
- `plonk/04-burning-transaction.sh` to burn the token and receive the reward.

You can also see all wallet at `05-show-all.sh`.

### Symbolic
