# Cardano integration in Asterizm flow


## CLI commands

Description of zkFold's CLI commands implementing Asterizm protocol can be consulted [here](./Asterizm.md).


## End-to-end Asterizm flow: BSC -> Cardano (destination)

This section explains how to implement an end-to-end Asterizm flow with Cardano as the destination chain. For concreteness, let us say that the source chain is BSC.  The usage of `asterizm` **zkfold-cli** commands inside that flow is described.

### Preliminaries

Assume that on EVM Asterizm we see:

1. Source **Client** contract creates a message (or message hash), initializes a transfer, and emits events.
2. **Relayer** observes, verifies, and calls **Relay** contract to "translate/confirm" the message on the destination chain.
3. Destination **Client** contract "receives" the message and performs some action (e.g., deliver to app logic, mark as processed, prevent replay).

In our current Cardano implementation, the destination-side has two key actions:

- Relayer attestation on Cardano:  `asterizm relayer`.  A Cardano relayer mints a token that *certifies a message hash*.  The token name encodes the message hash.
- Client reveal on Cardano: `asterizm client`.  The Cardano client consumes a UTxO containing the relayer token and creates an on-chain UTxO that contains the full plaintext message as datum, while validating:
   - the relayer is in the on-chain *Registry* (set up by `init`).
   - the plaintext message matches the certified hash in the relayer token-name.

(Note that an Asterizm-style destination "receive" that consumes/processes the message and prevents replay should be part of the final implementation.)

### Glosary

- **UTxO**: instead of account storage, Cardano state is a set of unspent outputs.  A transaction consumes UTxOs and creates new ones.
- **Datum**: structured data attached to a UTxO (often representing "state").
- **Minting policy**: a script that controls when a token can be minted/burned.
- **Policy ID**: token "contract id" (roughly analogous to ERC-20 contract address).
- **Token name**: per-policy token identifier.  In our system the message hash is embedded here (for relayer token).
- **Thread token**/NFT: a unique token used to identify a "singleton" UTxO (e.g. the *Registry*).

### End-to-end sequence overview (BSC -> Cardano)

#### Step (a) — initiate message on BSC

Goal: create a message on the source chain and get a stable identifier/hash.

Typical actions:
- `client.initMessage(payload)` or equivalent
- parse event: `(msgId, msgHash, dstChainId, dstAddress, ...)`

*Output for Cardano side*: at minimum the message bytes (payload) and/or its hash.  (In our Cardano tooling, the relayer expects a message hash file and the client expects the message plaintext file.)

#### Step (b) — initialize transfer on BSC client

Typical actions:
- `client.initTransfer(msgId, dstChainId, dstTrustedAddressUnit)`
- emit event used by relayers.

**Note**:  Asterizm uses a "trusted address" concepts (`uint`).  For Cardano, we'll need a project-wide convention for what "Cardano trusted address" means (more on this below).

#### Step (c) — Translate via relay + perform destination-side attestation/reveal

This step bridges EVM concepts into Cardano; zkfold-cli commands are used here.

##### (c1) (Optional) "Translate" on BSC relay

If EVM-side Asterizm expects a relay confirmation on the source chain:  relayer calls `relay.translate(msgId)` or equivalent.  This can be part of the "proof/confirmation pipeline".

##### (c2) Cardano: initialize destination protocol (one-time per client) — `init`

Before any messages can be certified and revealed, we must initialize the *Registry* of trusted relayers on Cardano.  Command:  `asterizm init`.  What this does:

- Creates a *Registry* UTxO on-chain that contains a datum listing allowed relayer policy IDs (whitelist).
- Mints a thread token (NFT) used to uniquely identify the Registry UTxO.
- Writes `./assets/asterizm-setup.json`, which contains parameters (client PKH + thread-token policy id) used by the client minting policy.  (Note that this bootstraps the destination client's minting policy so later `client` actions can validate relayers against the Registry.)

**Typical usage (once)**:
```shell
cabal run zkfold-cli:asterizm -- init \
  --signing-key-file payment.skey \
  --tx-oref <TxId#TxIx> \
  --registry-address <addr...> \
  --client-vkey-file client.vkey \
  --relayer-vkey-file relayer.vkey
```

**Inputs to understand**:
- `--tx-oref`: a UTxO reference used as entropy to mint the thread token (singleton).
- `--registry-address`: where the Registry UTxO lives.
- `--relayer-*`: defines the set of valid relayers (do not leave this empty).

**Outputs**:
- On-chain Registry UTxO + thread token.
- Local `assets/asterizm-setup.json` (needed for later steps).

At a high-level, we can view this as "deploy / configure destination contract + whitelist relayers".

##### (c3) Cardano: generate message artifacts — `message`

We need two artifacts:
- private message payload file (plaintext)
- public message-hash file

**Command**: `asterizm message`
```shell
cabal run zkfold-cli:asterizm -- message \
  --message-text "Hello from BSC to Cardano"
```

This writes by default:
- `assets/message.private` (payload bytes, private)
- `assets/message-hash.public` (hash, public)

Note that: a) the Cardano relayer only needs the hash (public); b) the Cardano client needs the plaintext (private) to reveal it on-chain.  In TypeScript integration, the TS code can produce payload bytes (from BSC event/log or from original input), then call this CLI step to generate consistent files.

##### (c4) Cardano: relayer certifies hash on-chain — `relayer`

This is the Carddano equivalent of "relay translation confirmation", implemented as *minting a certification token*.

**Command**: `asterizm relayer`
```shell
cabal run zkfold-cli:asterizm -- relayer \
  --signing-key-file relayer-payment.skey \
  --beneficiary-address <addr...> \
  --message-hash-file message-hash.public
```

What it does:
- Mints a relayer token under that relayer's policy.
- Token-name encodes the message hash.
- Places the token at `--beneficiary-address` (an address the protocol controls or monitors).

This is the destination-side *attestation object* proving "a trusted relayer says this hash is valid".  This can be viewed as "relayer posts proof/confirmation to destination chain".

##### (c5) Cardano: client reveals payload, validated agains Registry + hash — `client`

Now the destination client publishes the actual message payload on-chain, but only if:
- it consumes the relayer token UTxO, and
- validation passes.

**Command**: `asterizm client`
```shell
cabal run zkfold-cli:asterizm -- client \
  --signing-key-file client-payment.skey \
  --beneficiary-address <addr...> \
  --message-file message.private
```

What it does:
- Builds a tx that spends a UTxO containing the relayer token.
- Validates:
  1. relayer's policy id is in the on-chain Registry (thread-token points to it)
  2. `hash(message) == hash in relayer token-name`
- Mints a client token and creates an output containing the message as datum ("reveal").

This is the destination "message arrival" moment.  The message is now publicly visible on Cardano and tied to a trusted relayer attestation.  Can be interpreted as "destination client has accepted the message".

#### Observability: retrieve messages — `retrieve-messages`

This is for indexing/debugging and can be used in TypeScript code to poll.

**Command**: `asterizm retrieve-messages`
```shell
cabal run zkfold-cli:asterizm -- retrieve-messages
```

This gives a way to list/recover the revealed messages posted on-chain.  Can be though of like a lightweight "destination event reader".

#### Typical TypeScript implementation

A TS "operator script" (or backend service) can orchestrate the Asterizm flow as follows:

1. On BSC
   - send txs for (a) and (b)
   - listen to events `(msgId, msgHash, payload, dstParams...)`
2. On Cardano
   - ensure `init` already ran (bootstrap)
   - call `message` to derive `message.private` + `message-hash.public`
   - call `relayer` to mint attestation token
   - call `client` to reveal payload datum
   - call `retrieve-messages` to confirm arrival/for debugging
   - Finalize processing (receive).

#### Summary and integration notes

1. Cardano is UTxO-based.
   - In account (EVM) chains you "update storage".
   - In Cardano you "spend UTxOs and create new ones".  So the `init` step requires `--tx-oref` because minting the thread token and establishing singleton state uses a specific UTxO as input/entropy.

2. "Policy ID" is like "token contract id".
   Our *Registry* stores policy IDs of approved relayers.  So, on Cardano, "which relayers are trusted?" is expressed as "which minting policies are trusted to mint certification tokens".

3. The destination does not "call a contract" in the same way as in EVM chains.
   The  "receive" step will likely be a transaction that:
   - spends a UTxO locked by a validator
   - provides redeemer/datum
   - mints/burns token under policies

4. Where each *zkfold-cli* command fits in the Asterizm flow:
   - `init`: destination bootstrap: establish Registry + trusted relayers; generate stup file.
   - `message`: produce payload + hash artifacts used by the destination steps.
   - `relayer`: destination "relay translation/attestation"; mints certification token encoding message hash.
   - `client`: destination acceptance: validates relayer agains Registry; reveal payload datum on-chain.
   - `retrieve-messages`: destination read/indexing; confirm the message was revealed.


## End-to-end Asterizm flow: Cardano (source) -> BSC

(In progress...)
