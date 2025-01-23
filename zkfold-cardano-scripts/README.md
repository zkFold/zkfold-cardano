# zkFold: Smart Contracts
This repository contains Cardano L1 smart contracts.

## L1 Smart contracts
General description of smartcat tract

### ForwardingReward
The Plutus spending script that forwards verification to a rewarding script.

### ForwardingMint
The Plutus spending script that forwards verification to a minting script.

### PlonkVerifierToken
Plutus script (minting policy) for verifying computations on-chain.

### PlonkVerifierTx
Plutus script for verifying a ZkFold Symbolic smart contract on the current transaction.

### Rollup (in progress)
Plutus script for verifying a rollup state transition.

### RollupData (in progress)
Plutus script for posting rollup data on-chain.

### RollupDeposit (in progress)
Plutus proxy script for depositing funds into the rollup.

### Wallet
This script verifies that a transaction was either signed with a signature or has a ZKP associated with it allowing to access the funds.

## Benchmark of the Plonk verifier

Plonk benchmark results in `bench-cpu-mem`.

Rollup benchmark results in `bench-rollup`.
