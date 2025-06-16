# zkFold-cli

This directory houses CLI commands for the following projects:

- PlonkupVerifierToken
- Rollup
- PlonkupVerifierTx
- Balancing

High-level documentation for some of these can be found [here](https://github.com/zkFold/zkfold-cardano/tree/main/docs).

## Generalities

The *zkfold-cli* commands are implemented using the [Atlas](https://atlas-app.io/) framework.

Environment variable `CORE_CONFIG_PATH` must be initialized with the path to the file containing your configuration for network and provider. (File `config-template.json` provides a template configuration.)

A directory `./assets` will be created automatically by initialization commands.

After each transaction is executed, the estimated Tx fee and corresponding Tx ID will be displayed.

## zkFold-cli commands

The following is a list of available commands.  Note that **help** documentation for each cli command can be queried with, e.g.
```shell
cabal run zkfold-cli -- token-init --help
```

Note that only some of the options are mandatory; default values are chosen for the remainder.

### Asterizm

- `asterizm-mint`

### PlonkupVerifierTx

*(Temporarily disabled)*

- `plonkup-verifier-init`
- `plonkup-verifier-transfer`
- `plonkup-verifier-tx`

### PlonkupVerifierToken

- `token-init`              
- `token-transfer`           
- `token-mint`               
- `token-burn`               

### Rollup

- `rollup-init`              
- `rollup-update`            
- `rollup-clear`             

## Sample routine for *Asterizm*

### Minting

```shell
zkfold-cardano$ cabal run zkfold-cli -- asterizm-mint \
> --signing-key-file ../tests/keys/alice.skey \
> --beneficiary-address $(cat ../tests/keys/bob.addr) \
> --message "Hello, Asterizm!" \
> --submit-tx True

Estimated transaction fee: 206813 Lovelace
Transaction Id: 425452f6f7a440c7cabec6dcb9781d5b71fa63049880c0a8f099bf4eaeb1f805
```

## Sample routine for *PlonkupVerifierTx*

### Initialization

```shell
zkfold-cardano$ cabal run zkfold-cli -- plonkup-verifier-init

plonkupVerifierTx Address:
unsafeAddressFromText "addr_test1wptf204qe8t8uxw6ul09ta8uscrx5hw8rcv3secv2ylrapgz6zkax"
```

(To reset initialization, remove file `./assets/plonkupVerifierTx-setup-data.json`.)

### Transfer

Alice sent 5 Ada to the PlonkupVerifierTx script address:

```shell
zkfold-cardano$ cabal run zkfold-cli -- plonkup-verifier-transfer \
> --lovelace-reward 5000000 \
> --signing-key-file ../tests/keys/alice.skey \
> --change-address $(cat ../tests/keys/alice.addr)

Estimated transaction fee: 173245 Lovelace
Transaction Id: f99c96ebe0537619b84a841a2dc2d3154235e6a371e3c9429b8a36ace033bbf0
```

### Verifier transaction

With minimal flags, only input is the UTxO at the script address and there is no output (other than the change output, not considered in the plonkup verifier's input).  Transaction is built and it's fee is estimated, but by default it is not sent.

```shell
zkfold-cardano$ cabal run zkfold-cli -- plonkup-verifier-tx \
> --signing-key-file ../tests/keys/alice.skey \
> --change-address $(cat ../tests/keys/alice.addr)

Estimated transaction fee: 1162606 Lovelace
```

Let us now include one extra input and one output, this time submiting the transaction.  Note the higher fee.

```shell
zkfold-cardano$ cabal run zkfold-cli -- plonkup-verifier-tx \
> --signing-key-file ../tests/keys/alice.skey \
> --change-address $(cat ../tests/keys/alice.addr) \
> --tx-in 5134cdb516069ab0128d831621b38d0043239f7d0a684161acc9dd66d706b9c9#1 \
> --tx-out "$(cat ../tests/keys/alice.addr) + 5000000 lovelace" \
> --submit-tx True

Estimated transaction fee: 1177953 Lovelace
Transaction Id: 065e424251b28a771e64be27b7ff15cd6942becfae1b45074041e7503da485ac
```

In both cases, the proof was automatically computed and attached in the redeemer.

## Sample routine for *PlonkupVerifierToken*

### Initialization

```shell
zkfold-cardano$ cabal run zkfold-cli -- token-init \
> --signing-key-file ../tests/keys/alice.skey \
> --change-address $(cat ../tests/keys/alice.addr) \
> --parking-address $(cat ../tests/keys/bob.addr) \
> --tx-out-file token-init.tx
```

Parks `plonkupVerifierTokenCompiled` and `forwardingMintCompiled` scripts at chosen address.

### Transfer

```shell
zkfold-cardano$ cabal run zkfold-cli -- token-transfer \
> --lovelace-reward 7000000 \
> --signing-key-file ../tests/keys/alice.skey \
> --change-address $(cat ../tests/keys/alice.addr) \
> --tx-out-file token-transfer.tx
```

Transfers reward to `forwardingMint` address.

### Minting

```shell
zkfold-cardano$ cabal run zkfold-cli -- token-mint \
> --signing-key-file ../tests/keys/alice.skey \
> --change-address $(cat ../tests/keys/alice.addr) \
> --beneficiary-address $(cat ../tests/keys/charlie.addr) \
> --tx-id-file token-init.tx \
> --tx-out-file token-mint.tx
```

Mints reward token (representing cryptographic proof of some statement) and sends it to beneficiary.

### Burn & claim reward

```shell
zkfold-cardano$ cabal run zkfold-cli -- token-burn \
> --signing-key-file ../tests/keys/charlie.skey \
> --change-address $(cat ../tests/keys/charlie.addr) \
> --tx-id-file token-init.tx \
> --tx-out-file token-burn.tx
```

Token is burned to claim reward.

## Sample routine for *Rollup*

### Initialization

```shell
zkfold-cardano$ cabal run zkfold-cli -- rollup-init \
> --signing-key-file ../tests/keys/alice.skey \
> --change-address $(cat ../tests/keys/alice.addr) \
> --tx-oref 1046c2b3f52c284adb670bdf707074f6ce79d9a46889d0fb86248b0f17bccecf#1 \
> --fee-address $(cat ../tests/keys/bob.addr)
```

Initializes the rollup, creating a thread token and parking the `rollup` and `rollupData` scripts (three transactions).  Note that the reference to a specific (arbitrary) UTxO to be consumed needs to be specified with `--tx-oref`; this is needed to mint a unique thread token.

### Update

```shell
zkfold-cardano$ cabal run zkfold-cli -- rollup-update \
> --signing-key-file ../tests/keys/alice.skey \
> --change-address $(cat ../tests/keys/alice.addr)
```

Mints the necessary data tokens (one Tx per token) and executes the rollup update.  This command is meant to be executed recursively to move forward the rollups.

### Clear

```shell
zkfold-cardano$ cabal run zkfold-cli -- rollup-clear \
> --signing-key-file ../tests/keys/alice.skey \
> --change-address $(cat ../tests/keys/alice.addr)
```

This command can be run sporadically to burn the old (used) data tokens.
