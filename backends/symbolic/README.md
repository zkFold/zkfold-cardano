# Symbolic off-chain

1. `symbolic-init-transaction` writes the related Plutus scripts (UPLC code) to files in CBOR format.
2. `symbolic-transfer-transaction` writes token's policy id to a file in CBOR format.
3. `symbolic-withdraw-transaction` constructs the data items used as script parameters and writes them to files in CBOR format.