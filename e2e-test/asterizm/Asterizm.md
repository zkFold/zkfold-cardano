# Asterizm protocol CLI commands

This documents describes usage of our prototype implementation of the Asterizm protocol.

CLI commands are provided to

- compute the hash of a message
- derive client and relayer policy IDs
- relayer's certification of the message and its hash
- client's sending of outgoing messages to the blockchain
- client's receiving of incoming messages with relayer verification
- retrieval of messages posted on the blockchain

Message certification by a relayer is represented by minting of a token whose policy ID is derived from the relayer's verification key. Its token-name is the externally supplied hash of the message, and the message header is posted as inline datum. Regular SHA-256 is used by default by helper commands; pass `--crosschain-hash` to those helper/client commands when the Asterizm cross-chain hash is required. The client has a single policy ID for both incoming and outgoing proofs. Direction is selected by the minting redeemer, not by a separate client contract.

## Generalities

Our Asterizm CLI commands are implemented using the [Atlas](https://atlas-app.io/) framework.

A core configuration file is required for all commands that interact with the blockchain. This file contains your configuration for network and provider. (File `./config-template.json` provides a template configuration.)

After each transaction is executed, the Transaction ID will be displayed.

## Help documentation

CLI commands can be invoked with

```shell
cabal run zkfold-cli:asterizm -- <command> [options]
```

We can query the list of available commands:

```shell
cabal run zkfold-cli:asterizm -- --help
```

```output
zkfold-cli:asterizm - Command-line utility to interact with Cardano. Provides
specific commands to manage the 'Asterizm' protocol.

Usage: asterizm (client | hash | policy | relayer | retrieve-messages | user)

Available options:
  -h,--help                Show this help text

Available commands:
  client
  hash
  policy
  relayer
  retrieve-messages
  user
```

We now describe each command.  The eager reader can jump to [section "End-to-end test"](#end-to-end-test) below to see a sample workflow.

### hash

Computes a message hash. The message is provided as a HEX-encoded bytestring. By default this is regular SHA-256 over the supplied bytes. Pass `--crosschain-hash` to compute the Asterizm cross-chain hash instead.

```shell
cabal run zkfold-cli:asterizm -- hash --help
```

```output
Usage: asterizm hash [--crosschain-hash] --message HEX

Available options:
  --crosschain-hash        Use the Asterizm cross-chain hash instead of
                           regular SHA-256.
  --message HEX            Hex-encoded Asterizm structured message.
  -h,--help                Show this help text
```

```shell
cabal run zkfold-cli:asterizm -- hash --message HEX
```

```shell
cabal run zkfold-cli:asterizm -- hash --crosschain-hash --message HEX
```

### policy client

Derives and displays the client's policy ID. Does not interact with the blockchain.

Use the same `--client-vkey-file`, full `--relayer-vkey-file` list, and full `--trusted-address` list everywhere this client contract is referenced. These values are script parameters, so changing any of them derives a different policy ID.

```shell
cabal run zkfold-cli:asterizm -- policy client --help
```

```output
Usage: asterizm policy client --client-vkey-file FILEPATH
  [--relayer-vkey-file FILEPATH]
  [--trusted-address HEX]

Available options:
  --client-vkey-file FILEPATH
                           client's payment verification key file.
  --relayer-vkey-file FILEPATH
                           relayer's payment verification key file.
  --trusted-address HEX    Hex-encoded trusted Asterizm address: 8-byte chain
                           id followed by 32-byte address.
  -h,--help                Show this help text
```

### policy relayer

Derives and displays a relayer's policy ID. Does not interact with the blockchain.

```shell
cabal run zkfold-cli:asterizm -- policy relayer --help
```

```output
Usage: asterizm policy relayer --relayer-vkey-file FILEPATH

Available options:
  --relayer-vkey-file FILEPATH
                           relayer's payment verification key file.
  -h,--help                Show this help text
```

### policy user

Displays the universal user policy ID. This policy is not parameterized and is the same for all users. It is the policy used by `user send` (for example, by `03-user-outgoing.sh`). Does not interact with the blockchain.

```shell
cabal run zkfold-cli:asterizm -- policy user --help
```

```output
Usage: asterizm policy user

Available options:
  -h,--help                Show this help text
```

### policy token

Displays the omni-chain token policy ID. This policy is parameterized by the unified client proof policy and universal user-message policy. The token name is the empty bytestring.

```shell
cabal run zkfold-cli:asterizm -- policy token --help
```

```output
Usage: asterizm policy token --client-vkey-file FILEPATH
  [--relayer-vkey-file FILEPATH]
  [--trusted-address HEX]

Available options:
  --client-vkey-file FILEPATH
                           client's payment verification key file.
  --relayer-vkey-file FILEPATH
                           relayer's payment verification key file.
  --trusted-address HEX    Hex-encoded trusted Asterizm address: 8-byte chain
                           id followed by 32-byte address.
  -h,--help                Show this help text
```

### relayer

Command used by a relayer to mint a token certifying a client's message. The token-name is the supplied message hash, and the message header is posted as inline datum. Hashing is external to the relayer command.

```shell
cabal run zkfold-cli:asterizm -- relayer --help
```

```output
Usage: asterizm relayer --core-config-file FILEPATH
  --signing-key-file FILEPATH
  --relayer-vkey-file FILEPATH
  --beneficiary-address ADDRESS
  --message-header HEX
  --message-hash HEX

Available options:
  --core-config-file FILEPATH
                           Path to core config file (required).
  --signing-key-file FILEPATH
                           Payment signing key file.
  --relayer-vkey-file FILEPATH
                           relayer's payment verification key file.
  --beneficiary-address ADDRESS
                           Address of beneficiary receiving token(s).
  --message-header HEX     Hex-encoded Asterizm message header (excluding
                           payload).
  --message-hash HEX       Hex-encoded Asterizm message hash (32 bytes).
  -h,--help                Show this help text
```

### client send

Command used by client to send an outgoing message (Cardano as source chain). No relayer verification is required for the outgoing transaction, but relayer keys still participate in the unified client policy ID. The transaction must consume the user's universal-policy token for the same message hash and burn it while minting the client-policy proof token.

```shell
cabal run zkfold-cli:asterizm -- client send --help
```

```output
Usage: asterizm client send --core-config-file FILEPATH
  --signing-key-file FILEPATH
  --client-vkey-file FILEPATH
  [--relayer-vkey-file FILEPATH]
  [--trusted-address HEX]
  --beneficiary-address ADDRESS
  [--crosschain-hash]
  --message HEX

Available options:
  --core-config-file FILEPATH
                           Path to core config file (required).
  --signing-key-file FILEPATH
                           Payment signing key file.
  --client-vkey-file FILEPATH
                           client's payment verification key file.
  --relayer-vkey-file FILEPATH
                           relayer's payment verification key file.
  --trusted-address HEX    Hex-encoded trusted Asterizm address: 8-byte chain
                           id followed by 32-byte address.
  --beneficiary-address ADDRESS
                           Address of beneficiary receiving token(s).
  --crosschain-hash        Use the Asterizm cross-chain hash instead of
                           regular SHA-256.
  --message HEX            Hex-encoded Asterizm structured message.
  -h,--help                Show this help text
```

### user send

Command for any user to send a message to the blockchain. Unlike `client send`, this does not require a client verification key and does not enforce signature verification on-chain. All users share the same (universal) policy ID returned by `policy user`, not the client policy returned by `policy client`. A later `client send` transaction consumes and burns this token before minting the client-policy token.

When `--omni-policy-id` and `--omni-token-amt` are provided, `user send` also consumes one of the signer's UTxOs containing enough empty-name omni-chain tokens and places exactly that token amount in the same output as the user message token. This prepares the outgoing `client token burn` transaction.

```shell
cabal run zkfold-cli:asterizm -- user send --help
```

```output
Usage: asterizm user send --core-config-file FILEPATH
  --signing-key-file FILEPATH
  --beneficiary-address ADDRESS
  [--omni-policy-id HEX --omni-token-amt INTEGER]
  [--crosschain-hash]
  --message HEX

Available options:
  --core-config-file FILEPATH
                           Path to core config file (required).
  --signing-key-file FILEPATH
                           Payment signing key file.
  --beneficiary-address ADDRESS
                           Address of beneficiary receiving token(s).
  --omni-policy-id HEX     Omni-chain token policy ID. Must be paired with
                           --omni-token-amt.
  --omni-token-amt INTEGER Amount of empty-name omni-chain tokens to attach to
                           the user message UTxO.
  --crosschain-hash        Use the Asterizm cross-chain hash instead of
                           regular SHA-256.
  --message HEX            Hex-encoded Asterizm structured message.
  -h,--help                Show this help text
```

### client token mint

Command used by the client to receive an incoming omni-chain token transfer. It mints the unified client proof token with an incoming redeemer and the empty-name omni-chain tokens in the same transaction.

```shell
cabal run zkfold-cli:asterizm -- client token mint --help
```

```output
Usage: asterizm client token mint --core-config-file FILEPATH
  --signing-key-file FILEPATH
  --client-vkey-file FILEPATH
  [--relayer-vkey-file FILEPATH]
  [--trusted-address HEX]
  --beneficiary-address ADDRESS
  [--crosschain-hash]
  --message HEX

Available options:
  --core-config-file FILEPATH
                           Path to core config file (required).
  --signing-key-file FILEPATH
                           Payment signing key file.
  --client-vkey-file FILEPATH
                           client's payment verification key file.
  --relayer-vkey-file FILEPATH
                           relayer's payment verification key file.
  --trusted-address HEX    Hex-encoded trusted Asterizm address: 8-byte chain
                           id followed by 32-byte address.
  --beneficiary-address ADDRESS
                           Address of beneficiary receiving token(s).
  --crosschain-hash        Use the Asterizm cross-chain hash instead of
                           regular SHA-256.
  --message HEX            Hex-encoded Asterizm structured message.
  -h,--help                Show this help text
```

### client token burn

Command used by the client to approve an outgoing omni-chain token transfer. It consumes the UTxO containing both the user message token and the empty-name omni-chain tokens, burns both, and mints the unified client proof token with an outgoing redeemer.

```shell
cabal run zkfold-cli:asterizm -- client token burn --help
```

```output
Usage: asterizm client token burn --core-config-file FILEPATH
  --signing-key-file FILEPATH
  --client-vkey-file FILEPATH
  [--relayer-vkey-file FILEPATH]
  [--trusted-address HEX]
  --beneficiary-address ADDRESS
  [--crosschain-hash]
  --message HEX

Available options:
  --core-config-file FILEPATH
                           Path to core config file (required).
  --signing-key-file FILEPATH
                           Payment signing key file.
  --client-vkey-file FILEPATH
                           client's payment verification key file.
  --relayer-vkey-file FILEPATH
                           relayer's payment verification key file.
  --trusted-address HEX    Hex-encoded trusted Asterizm address: 8-byte chain
                           id followed by 32-byte address.
  --beneficiary-address ADDRESS
                           Address of beneficiary receiving token(s).
  --crosschain-hash        Use the Asterizm cross-chain hash instead of
                           regular SHA-256.
  --message HEX            Hex-encoded Asterizm structured message.
  -h,--help                Show this help text
```

### client receive

Command used by client to receive an incoming message (Cardano as destination chain). Requires relayer verification.

Transaction references UTxO with relayer's token. Client's minting policy validates *a)* the relayer's policy ID against the allowed set and *b)* compatibility between the client's message and the message-hash contained in the relayer's token-name.

```shell
cabal run zkfold-cli:asterizm -- client receive --help
```

```output
Usage: asterizm client receive --core-config-file FILEPATH
  --signing-key-file FILEPATH
  --client-vkey-file FILEPATH
  [--relayer-vkey-file FILEPATH]
  [--trusted-address HEX]
  --beneficiary-address ADDRESS
  [--crosschain-hash]
  --message HEX

Available options:
  --core-config-file FILEPATH
                           Path to core config file (required).
  --signing-key-file FILEPATH
                           Payment signing key file.
  --client-vkey-file FILEPATH
                           client's payment verification key file.
  --relayer-vkey-file FILEPATH
                           relayer's payment verification key file.
  --trusted-address HEX    Hex-encoded trusted Asterizm address: 8-byte chain
                           id followed by 32-byte address.
  --beneficiary-address ADDRESS
                           Address of beneficiary receiving token(s).
  --crosschain-hash        Use the Asterizm cross-chain hash instead of
                           regular SHA-256.
  --message HEX            Hex-encoded Asterizm structured message.
  -h,--help                Show this help text
```

### retrieve-messages

Command to retrieve client's messages posted (revealed) to the blockchain.

```shell
cabal run zkfold-cli:asterizm -- retrieve-messages --help
```

```output
Usage: asterizm retrieve-messages --core-config-file FILEPATH
  --client-vkey-file FILEPATH
  [--relayer-vkey-file FILEPATH]
  [--trusted-address HEX]
  (--incoming | --outgoing)

Available options:
  --core-config-file FILEPATH
                           Path to core config file (required).
  --client-vkey-file FILEPATH
                           client's payment verification key file.
  --relayer-vkey-file FILEPATH
                           relayer's payment verification key file.
  --trusted-address HEX    Hex-encoded trusted Asterizm address: 8-byte chain
                           id followed by 32-byte address.
  --incoming               Incoming cross-chain message (requires relayer
                           verification).
  --outgoing               Outgoing cross-chain message (no relayer verification
                           needed).
  -h,--help                Show this help text
```

## End-to-end test

What follows is a sample workflow illustrating usage of *Asterizm* CLI commands.  (Diagrams look best with your browser in *light mode*.)

The scripts use regular SHA-256 by default. To run the same flow with Asterizm cross-chain token names, set `CROSSCHAIN_HASH=1` on the relayer/client/user scripts that process the same message.

![workflow](figures/00-flow.svg)

**Figure:** Process flows for incoming messages (Cardano as destination) and outgoing messages (Cardano as source)

### Generate Keys

```shell
asterizm$ ./00-keygen.sh client
asterizm$ ./00-keygen.sh relayer
asterizm$ ./00-keygen.sh user
```

This generates verification and signing keys for the client, relayer, and user roles.

### Policy IDs

Derive the unified client and relayer policy IDs. Trusted addresses are encoded as `chainId(8 bytes) || address(32 bytes)`.

```shell
asterizm$ trustedAddress="000000000000000100000000000000000000000039d2ba91296029afbe725436b4824ca803e27391"

asterizm$ # Unified client policy ID for incoming and outgoing proofs
asterizm$ cabal run zkfold-cli:asterizm -- policy client \
  --client-vkey-file ./keys/client.vkey \
  --relayer-vkey-file ./keys/relayer.vkey \
  --trusted-address "$trustedAddress"
```

```output
<policy-id>
```

```shell
asterizm$ # Relayer policy ID
asterizm$ cabal run zkfold-cli:asterizm -- policy relayer \
  --relayer-vkey-file ./keys/relayer.vkey
```

```output
<policy-id>
```

### Relayer

The relayer mints a certification token for an incoming message and posts the message header as inline datum:

```shell
asterizm$ # Build message, then split header and hash externally
asterizm$ message="0000000000000001...48656c6c6f2c20417374657269...<<hex message>>"
asterizm$ messageHeader="${message:0:224}"
asterizm$ messageHash=$(cabal run zkfold-cli:asterizm -- hash --message "$message" | tr -d '"')

asterizm$ cabal run zkfold-cli:asterizm -- relayer \
  --core-config-file ./assets/config.json \
  --signing-key-file ./keys/relayer.skey \
  --relayer-vkey-file ./keys/relayer.vkey \
  --beneficiary-address $(cat ./keys/relayer.addr) \
  --message-header "$messageHeader" \
  --message-hash "$messageHash"
```

```output
"<transaction-id>"
```

Pass `--crosschain-hash` to `hash` here, and to the matching `client receive`, `client token mint`, `user send`, `client send`, or `client token burn` command below, when the token-name must use the Asterizm cross-chain hash.
This sample message has a 112-byte header, so `messageHeader` uses the first 224 hex characters. If your header includes extra flags, pass the complete header instead.

![relayer Tx](figures/03-relayer-tx.svg)

**Figure:** Relayer's Tx

### Client Token Mint (Incoming Message)

The client receives an incoming token-transfer message by validating the relayer's certification, minting the client proof token, and minting empty-name omni-chain tokens:

```shell
asterizm$ cabal run zkfold-cli:asterizm -- client token mint \
  --core-config-file ./assets/config.json \
  --signing-key-file ./keys/client.skey \
  --client-vkey-file ./keys/client.vkey \
  --relayer-vkey-file ./keys/relayer.vkey \
  --trusted-address "$trustedAddress" \
  --beneficiary-address $(cat ./keys/client.addr) \
  --message "$message"
```

```output
"<transaction-id>"
```

![client Tx](figures/04-client-tx.svg)

**Figure:** Client's Tx

### User Send (Outgoing Message)

Any user can initiate an outgoing message by minting and sending a token under the universal user policy:

*Note:* The resulting token is minted under `policy user`. If you are scanning for messages created by `user send`, do not derive `policy client` for that purpose.
For token transfers, pass the omni-chain policy ID and token amount so the user's message token and the selected omni-chain tokens are placed in the same UTxO. Run this step before `client token burn` for the same message.

```shell
asterizm$ omniPolicyId=$(cabal run zkfold-cli:asterizm -- policy token \
  --client-vkey-file ./keys/client.vkey \
  --relayer-vkey-file ./keys/relayer.vkey \
  --trusted-address "$trustedAddress")

asterizm$ cabal run zkfold-cli:asterizm -- user send \
  --core-config-file ./assets/config.json \
  --signing-key-file ./keys/client.skey \
  --beneficiary-address $(cat ./keys/client.addr) \
  --omni-policy-id "$omniPolicyId" \
  --omni-token-amt 100 \
  --message "$outgoingMessage"
```

```output
"<transaction-id>"
```

### Client Token Burn (Outgoing Message)

After a user has posted an outgoing token-transfer message under `policy user`, the client approves it by minting the corresponding unified client-policy token with an outgoing redeemer. The transaction consumes the user's token UTxO, burns the user message token, burns the omni-chain token amount from the payload, and checks the destination trusted address on-chain:

```shell
asterizm$ cabal run zkfold-cli:asterizm -- client token burn \
  --core-config-file ./assets/config.json \
  --signing-key-file ./keys/client.skey \
  --client-vkey-file ./keys/client.vkey \
  --relayer-vkey-file ./keys/relayer.vkey \
  --trusted-address "$trustedAddress" \
  --beneficiary-address $(cat ./keys/client.addr) \
  --message "$outgoingMessage"
```

```output
"<transaction-id>"
```

![client send Tx](figures/05-client-send-tx.svg)

**Figure:** Client Token Burn Tx

### Retrieve Messages

```shell
asterizm$ # Retrieve incoming messages
asterizm$ cabal run zkfold-cli:asterizm -- retrieve-messages \
  --core-config-file ./assets/config.json \
  --client-vkey-file ./keys/client.vkey \
  --relayer-vkey-file ./keys/relayer.vkey \
  --trusted-address "$trustedAddress" \
  --incoming

asterizm$ # Retrieve outgoing messages
asterizm$ cabal run zkfold-cli:asterizm -- retrieve-messages \
  --core-config-file ./assets/config.json \
  --client-vkey-file ./keys/client.vkey \
  --relayer-vkey-file ./keys/relayer.vkey \
  --trusted-address "$trustedAddress" \
  --outgoing
```

```output
Client's messages on-chain:

<token-transfer message bytes>
```

*Note:* `retrieve-messages` derives the client's policy ID from the client key, relayer keys, and trusted addresses, then filters datums by message direction. It only returns messages minted via `client send`, `client receive`, `client token mint`, or `client token burn`. It does not retrieve transactions created by `user send`.

---

*Note:*  You can reproduce this workflow using the shell scripts provided in directory `./e2e-test/asterizm`.  (Make this your active directory.)  Generate keys using `./00-keygen.sh client`, `./00-keygen.sh relayer`, and `./00-keygen.sh user`, then fund the client, relayer, and user addresses before running the scripts. The token-transfer outgoing script signs `user send` with the client key so it can attach the selected amount from the client's omni-chain token UTxO to the user message token. If your default compiler is not the project compiler, run scripts with `CABAL_FLAGS=--with-compiler=ghc-9.6.7`. Run the numbered scripts in order; `./04-client-outgoing.sh` depends on the universal-policy token posted by `./03-user-outgoing.sh`.
