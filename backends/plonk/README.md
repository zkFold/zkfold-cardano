# Plonk off-chain

1. `plonk-init-transaction` writes the related Plutus scripts (UPLC code) to files in CBOR format.
2. `plonk-minting-transaction` constructs the data items used as script parameters and writes them to files in CBOR format.
3. `plonk-transfer-transaction` writes token's policy id to a file in CBOR format.