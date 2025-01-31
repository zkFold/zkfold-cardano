# PlonkupVerifierTx off-chain

1. `plonkupVerifierTx-init-transaction` writes the related Plutus scripts (UPLC code) to files in CBOR format.
2. `plonkupVerifierTx-transfer-transaction` writes token's policy id to a file in CBOR format.
3. `plonkupVerifierTx-withdraw-transaction` constructs the data items used as script parameters and writes them to files in CBOR format.