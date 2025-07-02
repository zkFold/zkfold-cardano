# Asterizm protocol CLI commands

This documents describes usage of our prototype implementation of the Asterizm protocol.

CLI commands are provided to

- initialize the protocol
- client's generation of private/public files associated with a specific message
- relayer's certification of the message's hash
- client's sending of message to the blockchain following a valid relayer's certification
- retrival of messages posted on the blockchain

Message certification by a relayer is represented by minting of a token whose policy id is defined at initialization.  Its token-name is the hash of the message.  Another token is also minted when client sends the original message to the blockchain, requiring validation that the token-name of a token minted by a valid relayer corresponds to the hash of the message.

## Generalities

Our Asterizm CLI commands are implemented using the [Atlas](https://atlas-app.io/) framework.

Environment variable `CORE_CONFIG_PATH` must be initialized with the path to the file containing your configuration for network and provider:

```shell
export CORE_CONFIG_PATH=<path to your config file>
```
(File `./config-template.json` provides a template configuration.)

After each transaction is executed, the estimated Tx fee and corresponding Tx ID will be displayed.

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

Usage: asterizm (init | client | message | relayer | retrieve-messages)

Available options:
  -h,--help                Show this help text

Available commands:
  init                     
  client                   
  message                  
  relayer                  
  retrieve-messages        
```

We now describe each command.  The eager reader can jump to [section "End-to-end test"](#end-to-end-test) below to see a sample workflow.

### init

Initializes the protocol for a specific client.  A set of valid relayers is provided at this point.  The command posts at the blockchain, as datum, a list of policy-id's associated to the valid relayer's.  We call this list "the Registry".  A specific reference to an arbitrary UTxO to be consumed needs to be provided, as it is needed to generate the nft (thread token) that will identify the Registry.

A directory `./assets` will be created by this command.

```shell
cabal run zkfold-cli:asterizm -- init --help
```

```output
Usage: asterizm init [--core-config-file FILEPATH]
  (--signing-key HEX | --signing-key-file FILEPATH)
  --tx-oref TxId#TxIx
  --registry-address ADDRESS
  (--client-pkh HEX | --client-vkey-file FILEPATH)
  [--relayer-pkh HEX | --relayer-vkey-file FILEPATH]
  [--init-out-file FILEPATH]

Available options:
  --core-config-file FILEPATH
                           Path to 'core config'. This overrides the
                           CORE_CONFIG_PATH environment variable. The argument
                           is optional if CORE_CONFIG_PATH is defined and
                           mandatory otherwise.
  --signing-key HEX        Hex-encoded signing key
  --signing-key-file FILEPATH
                           Payment signing key file.
  --tx-oref TxId#TxIx      TxOutRef (reference to a Tx input) to be consumed at
                           initialization.
  --registry-address ADDRESS
                           Address to park relayer's registry at.
  --client-pkh HEX         Hex-encoded client's pub-key-hash.
  --client-vkey-file FILEPATH
                           Client's payment verification-key file.
  --relayer-pkh HEX        Hex-encodec relayer's pub-key-hash.
  --relayer-vkey-file FILEPATH
                           Relayer's payment verification-key file.
  --init-out-file FILEPATH Path (relative to 'assets/') for Asterizm
                           initialization tx out-file.
  -h,--help                Show this help text
```

Note that, eventhough providing the relayers is optional, not providing any (an empty list) would render a useless protocol.

### message

Generates the private/public files associated to a given message.  The message can be provided as either a plain ASCII string or as an HEX-encoded bytestring.

```shell
cabal run zkfold-cli:asterizm -- message --help
```

```output
Usage: asterizm message (--message-text ASCII | --message-hex HEX)
  [--message-file FILEPATH]
  [--message-hash-file FILEPATH]

Available options:
  --message-text ASCII     Asterizm message as string of ASCII characters.
  --message-hex HEX        Hex-encoded Asterizm message.
  --message-file FILEPATH  Path (relative to 'assets/') for PRIVATE file storing
                           message. (default: "message.private")
  --message-hash-file FILEPATH
                           Path (relative to 'assets/') for PUBLIC file storing
                           message hash. (default: "message-hash.public")
  -h,--help                Show this help text
```

### relayer

Command used by a relayer to mint a token certifying a client's message.

```shell
cabal run zkfold-cli:asterizm -- relayer --help
```

```output
Usage: asterizm relayer [--core-config-file FILEPATH]
  (--signing-key HEX | --signing-key-file FILEPATH)
  --beneficiary-address ADDRESS
  [--message-hash-file FILEPATH]
  [--relayer-out-file FILEPATH]

Available options:
  --core-config-file FILEPATH
                           Path to 'core config'. This overrides the
                           CORE_CONFIG_PATH environment variable. The argument
                           is optional if CORE_CONFIG_PATH is defined and
                           mandatory otherwise.
  --signing-key HEX        Hex-encoded signing key
  --signing-key-file FILEPATH
                           Payment signing key file.
  --beneficiary-address ADDRESS
                           Address of beneficiary receiving token(s).
  --message-hash-file FILEPATH
                           Path (relative to 'assets/') for PUBLIC file storing
                           message hash. (default: "message-hash.public")
  --relayer-out-file FILEPATH
                           Path (relative to 'assets/') for Asterizm
                           relayer-mint tx out-file.
  -h,--help                Show this help text
```

### client

Command used by client to post (reveal) original message to the blockchain.

```shell
cabal run zkfold-cli:asterizm -- client --help
```

```output
Usage: asterizm client [--core-config-file FILEPATH]
  (--signing-key HEX | --signing-key-file FILEPATH)
  --beneficiary-address ADDRESS
  [--message-file FILEPATH]
  [--client-out-file FILEPATH]

Available options:
  --core-config-file FILEPATH
                           Path to 'core config'. This overrides the
                           CORE_CONFIG_PATH environment variable. The argument
                           is optional if CORE_CONFIG_PATH is defined and
                           mandatory otherwise.
  --signing-key HEX        Hex-encoded signing key
  --signing-key-file FILEPATH
                           Payment signing key file.
  --beneficiary-address ADDRESS
                           Address of beneficiary receiving token(s).
  --message-file FILEPATH  Path (relative to 'assets/') for PRIVATE file storing
                           message. (default: "message.private")
  --client-out-file FILEPATH
                           Path (relative to 'assets/') for Asterizm client-mint
                           tx out-file.
  -h,--help                Show this help text
```

### retrieve-messages

Command to retrieve client's messages posted (revealed) to the blockchain.

```shell
cabal run zkfold-cli:asterizm -- retrieve-messages --help
```

```output
Usage: asterizm retrieve-messages [--core-config-file FILEPATH]

Available options:
  --core-config-file FILEPATH
                           Path to 'core config'. This overrides the
                           CORE_CONFIG_PATH environment variable. The argument
                           is optional if CORE_CONFIG_PATH is defined and
                           mandatory otherwise.
  -h,--help                Show this help text
```

(No options need to be given if `CORE_CONFIG_PATH` is already an environment variable.)

## End-to-end test

What follows is a sample workflow illustrating usage of *Asterizm* CLI commands.

### Initialization

```shell
asterizm$ cabal run zkfold-cli:asterizm -- init \
  --signing-key-file ./keys/someone.skey \
  --tx-oref 4e16927f137a748a1ac7f19c26166695cb8a0031f1fc16b098d793f357aeb13f#0 \
  --registry-address $(cat ./keys/asterizm.addr) \
  --client-vkey-file ./keys/alice.vkey \
  --relayer-vkey-file ./keys/bob.vkey \
  --relayer-vkey-file ./keys/charlie.vkey \
  --relayer-vkey-file ./keys/dylan.vkey
```

```output
Estimated transaction fee: 397683 Lovelace
Transaction Id: 9f17b5d93780c7a7149e53cbde2747515a868748259235554917635c41304472
```

### Message

```shell
asterizm$ cabal run zkfold-cli:asterizm -- message \
  --message-text "Hello, Asterizm!"
```

```output
Saving Asterizm message (private file: message.private)...
Saving message hash (public file: message-hash.public)...
Done.
```

### Relayer

```shell
asterizm$ cabal run zkfold-cli:asterizm -- relayer \
  --signing-key-file ./keys/bob.skey \
  --beneficiary-address $(cat ./keys/bob.addr)
```

```output
Estimated transaction fee: 884420 Lovelace
Transaction Id: 34b5073da5da6869dddc24b3b2d75c3a1c3ea90769d944c2170ec6aee99af5e1
```

### Client

```shell
asterizm$ cabal run zkfold-cli:asterizm -- client \
  --signing-key-file ./keys/alice.skey \
  --beneficiary-address $(cat ./keys/alice.addr)
```

```output
Estimated transaction fee: 1383689 Lovelace
Transaction Id: bde3ddf213cb648f66b7c0ed5242d825263cd2fe2a842679b2d990ddcf4ec37b
```

### Retrieve Messages

```shell
asterizm$ cabal run zkfold-cli:asterizm -- retrieve-messages
```

```output
Client's messages on-chain:

B "Hello, Asterizm!"
```

*Note:*  You can reproduce this workflow using the shell files provided in directory `./e2e-test/asterizm`.  (Make this your active directory.)  It is assumed that you have independently generated .addr, .skey and .vkey files for users "someone", "asterizm", "alice", "bob", "charlie" and "dylan" in directory `./e2e-test/asterizm/keys` and that you have funded "someone", "alice" and "bob".
