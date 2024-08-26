# Cli scripts


## Docs

We have a description of transactions in `src-docs`.


## Scripts

Here we describe usage of bash scripts for executing transactions on the sancho.network testnet.

It is assumed that you have *cardano-node* running and have executed

```shell
export CARDANO_NODE_SOCKET_PATH=<path-to-node.socket>
```
on your shell.

### Plonk

To initialize the system:

- Execute `./plonk/00a-init-addrs.sh` to initialize needed wallets.  The address of *someone*'s wallet will be displayed.
- Fund *someone*'s wallet (typically using Sancho-net's public Faucet).
- Execute `./plonk/00b-init-funding.sh` to fund the remaining wallets.  (Alternatively, transfer manually some Ada to all generated wallets, but make sure *someone*'s wallet gets at least two UTxO's with funds.)


To perform the transactions, execute the following 4 scripts.

- `./plonk/01-init-transaction.sh` to publish Plutus scripts on the blockchain.
- `./plonk/02-transfer-transaction.sh` to set up a reward for burning a token.
- `./plonk/03-minting-transaction.sh` to mint and send a token to the owner.
- `./plonk/04-burning-transaction.sh` to burn the token and receive the reward.

You can also see the state of all wallets with `./plonk/05-show-all.sh`.

### Symbolic

