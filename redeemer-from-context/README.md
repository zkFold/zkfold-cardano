# Create redeemer

`cabal run redeemer-from-context` to run cardano-cli parser to create some avalible part of ScriptContext for off-chain.

code example from (scripts cli):

```
cabal run redeemer-from-context -- conway transaction build \
    --testnet-magic 4 \
    --tx-in $script \
    --spending-tx-in-reference $reference \
    --spending-plutus-script-v3 \
    --spending-reference-tx-in-redeemer-file "$assets/unit.json" \
    --spending-reference-tx-in-datum-file "$assets/unit.json" \
    --tx-in-collateral $collateral \
    --tx-out "$(cat $keypath/bob.addr) + 50000000 lovelace" \
    --change-address "$(cat $keypath/someone.addr)" \
    --out-file "$keypath/someone.txbody"
```

For conway era and v3, there is no complete backend yet, so it contains a lot of API internals.