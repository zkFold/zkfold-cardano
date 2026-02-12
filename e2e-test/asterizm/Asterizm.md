# Asterizm protocol CLI commands

This documents describes usage of our prototype implementation of the Asterizm protocol.

CLI commands are provided to

- compute the hash of a message
- relayer's certification of the message's hash
- client's sending of outgoing messages to the blockchain
- client's receiving of incoming messages with relayer verification
- retrieval of messages posted on the blockchain

Message certification by a relayer is represented by minting of a token whose policy ID is derived from the relayer's verification key. Its token-name is the hash of the message. Another token is also minted when client sends the original message to the blockchain, requiring validation that the token-name of a token minted by a valid relayer corresponds to the hash of the message.

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

Usage: asterizm (client | hash | relayer | retrieve-messages)

Available options:
  -h,--help                Show this help text

Available commands:
  client                   
  hash                     
  relayer                  
  retrieve-messages        
```

We now describe each command.  The eager reader can jump to [section "End-to-end test"](#end-to-end-test) below to see a sample workflow.

### hash

Computes the hash of a given message. The message is provided as a HEX-encoded bytestring.

```shell
cabal run zkfold-cli:asterizm -- hash --help
```

```output
Usage: asterizm hash --message HEX

Available options:
  --message HEX            Hex-encoded Asterizm structured message.
  -h,--help                Show this help text
```

### relayer

Command used by a relayer to mint a token certifying a client's message.

```shell
cabal run zkfold-cli:asterizm -- relayer --help
```

```output
Usage: asterizm relayer --core-config-file FILEPATH
  --signing-key-file FILEPATH
  --relayer-vkey-file FILEPATH
  --beneficiary-address ADDRESS
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
  --message-hash HEX       Hex-encoded Asterizm message hash (32 bytes).
  -h,--help                Show this help text
```

### client send

Command used by client to send an outgoing message (Cardano as source chain). No relayer verification is required.

```shell
cabal run zkfold-cli:asterizm -- client send --help
```

```output
Usage: asterizm client send --core-config-file FILEPATH
  --signing-key-file FILEPATH
  --client-vkey-file FILEPATH
  --beneficiary-address ADDRESS
  --message HEX

Available options:
  --core-config-file FILEPATH
                           Path to core config file (required).
  --signing-key-file FILEPATH
                           Payment signing key file.
  --client-vkey-file FILEPATH
                           client's payment verification key file.
  --beneficiary-address ADDRESS
                           Address of beneficiary receiving token(s).
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
  --beneficiary-address ADDRESS
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
  --beneficiary-address ADDRESS
                           Address of beneficiary receiving token(s).
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
  (--incoming | --outgoing)

Available options:
  --core-config-file FILEPATH
                           Path to core config file (required).
  --client-vkey-file FILEPATH
                           client's payment verification key file.
  --relayer-vkey-file FILEPATH
                           relayer's payment verification key file.
  --incoming               Incoming cross-chain message (requires relayer
                           verification).
  --outgoing               Outgoing cross-chain message (no relayer verification
                           needed).
  -h,--help                Show this help text
```

## End-to-end test

What follows is a sample workflow illustrating usage of *Asterizm* CLI commands.  (Diagrams look best with your browser in *light mode*.)

![workflow](figures/00-flow.svg)

**Figure:** process flow

### Generate Keys

```shell
asterizm$ ./00-keygen.sh client
asterizm$ ./00-keygen.sh relayer
```

This generates verification and signing keys for the client and relayer roles.

### Relayer

The relayer mints a certification token for an incoming message:

```shell
asterizm$ # Build message and compute hash
asterizm$ message="0000000000000001...48656c6c6f2c20417374657269...<<hex message>>"
asterizm$ messageHash=$(cabal run zkfold-cli:asterizm -- hash --message "$message" | tr -d '"')

asterizm$ cabal run zkfold-cli:asterizm -- relayer \
  --core-config-file ./assets/config.json \
  --signing-key-file ./keys/relayer.skey \
  --relayer-vkey-file ./keys/relayer.vkey \
  --beneficiary-address $(cat ./keys/relayer.addr) \
  --message-hash "$messageHash"
```

```output
"<transaction-id>"
```

![relayer Tx](figures/03-relayer-tx.svg)

**Figure:** Relayer's Tx

### Client Receive (Incoming Message)

The client receives an incoming message by validating the relayer's certification:

```shell
asterizm$ cabal run zkfold-cli:asterizm -- client receive \
  --core-config-file ./assets/config.json \
  --signing-key-file ./keys/client.skey \
  --client-vkey-file ./keys/client.vkey \
  --relayer-vkey-file ./keys/relayer.vkey \
  --beneficiary-address $(cat ./keys/client.addr) \
  --message "$message"
```

```output
"<transaction-id>"
```

![client Tx](figures/04-client-tx.svg)

**Figure:** Client's Tx

### Client Send (Outgoing Message)

The client sends an outgoing message (no relayer verification needed):

```shell
asterizm$ cabal run zkfold-cli:asterizm -- client send \
  --core-config-file ./assets/config.json \
  --signing-key-file ./keys/client.skey \
  --client-vkey-file ./keys/client.vkey \
  --beneficiary-address $(cat ./keys/client.addr) \
  --message "$outgoingMessage"
```

```output
"<transaction-id>"
```

### Retrieve Messages

```shell
asterizm$ # Retrieve incoming messages
asterizm$ cabal run zkfold-cli:asterizm -- retrieve-messages \
  --core-config-file ./assets/config.json \
  --client-vkey-file ./keys/client.vkey \
  --relayer-vkey-file ./keys/relayer.vkey \
  --incoming

asterizm$ # Retrieve outgoing messages
asterizm$ cabal run zkfold-cli:asterizm -- retrieve-messages \
  --core-config-file ./assets/config.json \
  --client-vkey-file ./keys/client.vkey \
  --outgoing
```

```output
Client's messages on-chain:

B "Hello, Asterizm!"
```

---

*Note:*  You can reproduce this workflow using the shell scripts provided in directory `./e2e-test/asterizm`.  (Make this your active directory.)  Generate keys using `./00-keygen.sh client` and `./00-keygen.sh relayer`, then fund the client and relayer addresses before running the scripts.
