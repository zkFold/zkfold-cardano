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
   - the relayer's policy ID is in the set of allowed relayers (embedded in the client's minting policy at setup time).
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

Before any messages can be certified and revealed, we must configure the set of trusted relayers.  Command:  `asterizm init`.  What this does:

- Computes the policy IDs for each allowed relayer based on their public key hash.
- Writes `./assets/asterizm-setup.json`, which contains parameters (client PKH + list of allowed relayer policy IDs) used by the client minting policy.

Note: This is an **off-chain** operation. The allowed relayers are embedded directly in the client's minting policy, eliminating the need for an on-chain registry.

**Typical usage (once)**:
```shell
cabal run zkfold-cli:asterizm -- init \
  --client-vkey-file client.vkey \
  --relayer-vkey-file relayer.vkey
```

**Inputs to understand**:
- `--relayer-*`: defines the set of valid relayers (do not leave this empty).

**Outputs**:
- Local `assets/asterizm-setup.json` (needed for later steps).

At a high-level, we can view this as "configure destination client + whitelist relayers".

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
- Builds a tx that references a UTxO containing the relayer token.
- Validates:
  1. relayer's policy id is in the allowed set (embedded in client's minting policy)
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

This section explains how to integrate Cardano as a *source* chain into the Asterizm protocol. For concreteness, let us say that the destination chain is BSC.

The key idea is to replace the usual EVM-side *event emission* (`SendMessageEvent`) with a *Cardano transaction that publishes relay metadata as a datum* at a known script address. A relayer then observes this transaction and performs the standard Asterizm flow on BSC.

### 1. Conceptual mapping: EVM vs Cardano (source side)

| EVM source chain                        | Cardano source chain                             |
| --------------------------------------- | ------------------------------------------------ |
| Smart contract emits `SendMessageEvent` | Client submits a transaction to a script address |
| Event logs are indexed                  | UTxOs at a known script address are indexed      |
| `txHash` identifies the event           | Cardano `txId` identifies the datum              |
| Message authenticity often implicit     | Must be *explicitly attested* by relayer         |

On Cardano, there is no *event log*. Instead, *data is published by creating a UTxO*. This UTxO plays the role of “message intent”.

### 2. High-level architecture

#### Components

1. **Client off-chain module (Cardano)**
   - Builds the relay payload.
   - Computes a `messageId` (known *before* submission).
   - Submits a Cardano transaction paying the transfer fee and attaching the payload as datum.

2. **Fee vault validator (Cardano)**
   - A minimal *Plutus script*.
   - Accepts ADA from anyone.
   - Can only be spent by an *Asterizm administrator* (parameterized by a payment key hash).
   - Does not enforce a datum structure constraint (by design, for now).

3. **Relayer (off-chain)**
   - Monitors a Cardano chain indexer for outputs sent to the fee vault address.
   - Extracts the datum.
   - Attests that the message originated on Cardano.
   - Executes the normal Asterizm flow on BSC.

### 3. Relay payload (datum) structure

When Cardano is the source chain, the relay payload contains:

- `srcChainId` – Cardano chain id
- `srcAddress` – *payment key hash* of the Cardano sender
- `dstChainId` – BSC chain id
- `dstAddress` – destination client contract on BSC
- `messageId` – precomputed unique identifier (see below)
- `transferResultNotifyFlag` – whether an acknowledgement is expected
- `transferHash` – hash of the payload per Asterizm rules

#### `srcAddress` on Cardano

We recommend:

- *Payment key hash (PKH)* of the wallet that submits the transaction.

This gives a stable, chain-native sender identity analogous to an EVM address.

### 4. Identifiers: `messageId` vs Cardano `txId`

Introduce a `messageId` that is known up-front, and treat the Cardano `txId` as *transport metadata*, e.g. `messageId = blake2b_256(clientNonce || payloadHash || dstChainId || dstAddress || ...)`.

- `messageId`
  - Computed off-chain by the client.
  - Used for replay protection on the destination chain.
- Cardano `txId`
  - Known only after submission.
  - Recorded by the relayer when it observes the transaction.

This avoids circular dependencies when building Cardano transactions and works well with wallet-based signing.

### 5. Submitting the source transaction (`init-relay`)

The client publishes the relay intent using **zkfold-cli**:

```shell
cabal run zkfold-cli:asterizm -- init-relay \
  --signing-key-file payment.skey \
  --init-src-chain-id <cardanoChainId> \
  --init-src-address <payment-key-hash> \
  --init-dst-chain-id 97 \
  --init-dst-address <bsc-client-address> \
  --init-message-id <hex-message-id> \
  --init-notify-flag true \
  --init-transfer-hash <hex-transfer-hash>
```

Note that `--init-src-chain-id` can have a default value equal to the Cardano chain id agreed upon in the Asterizm protocol.

Note also that the transaction:
- Pays the transfer fee in ADA.
- Sends it to the *fee vault script address*.
- Attaches the relay payload as an *inline datum*.

At this point, Cardano has done its job: the “event” has been published.

### 6. Relayer: monitoring Cardano

The relayer continuously monitors a Cardano indexer (e.g. Maestro, Blockfrost, or a local node) for *outputs sent to the fee vault address*.

For each matching UTxO, it:
1. Reads the datum.
2. Extracts `messageId`, routing info, and payload hash.
3. Records the Cardano `txId` off-chain.

> If a *marker token* is added in a later version, the relayer can additionally (or alternatively) monitor for outputs containing that token, which may improve robustness and indexing efficiency.

### 7. Authenticity: why BSC should trust the message

Current model — the relayer (or a small committee of relayers):

- Attests: "observed a Cardano transaction with txId X at the Asterizm fee vault address, carrying datum D.”
- Signs this statement.
- Submits it to BSC as part of the Asterizm relay call.

On BSC:

- The relay / initializer contract verifies the relayer signature(s).
- This establishes that the message genuinely originated on Cardano.

A later version can combine:

- Federation attestation, *plus*
- Stronger on-chain replay protection and/or partial chain proofs.

### 8. Destination chain (BSC): replay protection

On BSC, the destination logic must:

- Store `messageId -> consumed = true`.
- Reject any attempt to process a `messageId` more than once.

Note that if a marker token + nonce is introduced on Cardano (in a future version), this may further strengthen replay protection and enforce uniqueness already at the source.

### 9. Full end-to-end flow (Cardano -> BSC)

1. **Client prepares payload**
   - Computes `messageId` and `transferHash`.
   
2. **Client submits Cardano transaction**
   - Uses `init-relay`.
   - Pays ADA fee.
   - Attaches payload as datum.
   
3. **Relayer observes Cardano**
   - Detects UTxO at fee vault address.
   - Records Cardano `txId`.
   
4. **Relayer attests**
   - Signs observation of Cardano message.
   
5. **Relayer calls BSC**
   - Executes Asterizm relay / initializer flow.
   
6. **BSC client receives**
   - Verifies relayer attestation.
   - Checks `messageId` replay protection.
   - Executes `asterizmIzReceive` / `asterizmCiReceive`.
   
7. **Optional notification**
   - If `notifyFlag` is set, result can be reported back off-chain or on-chain.

### 10. Design notes & future extensions

- The fee vault validator does not enforce datum structure. This keeps the script minimal and flexible.

- Marker token (future) — adding a marker/token-minting step could:
  - Improve relayer indexing.
  - Enable on-chain uniqueness constraints.
  - Allow datum structure checks in the minting policy.

- Symmetry with EVM flows — conceptually, this Cardano-source flow mirrors EVM-source Asterizm:
  - *UTxO creation* replaces *event emission*.
  - *Relayer attestation* replaces *implicit log trust*.
