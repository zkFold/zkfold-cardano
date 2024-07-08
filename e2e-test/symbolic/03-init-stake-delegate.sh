#! /bin/bash

keypath=./keys
assets=../assets

echo ""
echo "alice wants to send money to bob, but only if someone can prove that he is an adult."
echo ""

symbolicVerifier=$(cardano-cli transaction txid --tx-file "$keypath/symbolicVerifier.tx")#0
setup=$(cardano-cli transaction txid --tx-file "$keypath/setup.tx")#0
alice=$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
collateral=$(cardano-cli query utxo --address $(cat $keypath/someone.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')

echo "alice address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/alice.addr) --testnet-magic 4)"
echo ""











cardano-cli conway stake-address build \
    --stake-script-file "$assets/symbolicVerifier.plutus" \
    --out-file "$keypath/symbolicVerifierStaking.addr" \
    --testnet-magic 4

# We have a fully functioning stake pool at this point. We now want to test Plutus staking script withdrawals.

# We now create the Plutus script staking registration certificate

cardano-cli conway stake-address registration-certificate \
  --testnet-magic 4 \
  --stake-script-file "$assets/symbolicVerifier.plutus" \
  --out-file "$keypath/symbolicVerifier.regcert"

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/someone.addr)" \
    --tx-in $in \
    --certificate-file "$keypath/symbolicVerifier.regcert" \
    --out-file "$keypath/register-plutus-staking-script.txbody"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/register-plutus-staking-script.txbody" \
    --signing-key-file "$keypath/bob.skey" \
    --out-file "$keypath/register-plutus-staking-script.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/register-plutus-staking-script.tx"

#-------------------------------------------------------------------------------

#Create delegation certificate for Plutus staking script to stake pool

cardano-cli conway stake-address registration-certificate \
  --testnet-magic 4 \
  --stake-script-file "$assets/symbolicVerifier.plutus" \
  --cold-verification-key-file "$keypath/cold.vkey" \
  --out-file "$keypath/plutus-script.delegcert"


#Delegate Plutus staking script to stake pool

cardano-cli conway query protocol-parameters \
   --testnet-magic 4 \
   --out-file "$keypath/pparams.json"

plutusStakingScriptRedeemer=$undefined

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/someone.addr)" \
    --tx-in $in \
    --tx-in-collateral $txinCollateral1 \
    --tx-out "$(cat $keypath/symbolicVerifier.addr) + 10000000 lovelace" \
    --certificate-file "$keypath/plutus-script.delegcert" \
    --certificate-script-file "$assets/symbolicVerifier.plutus" \
    --certificate-redeemer-file plutusStakingScriptRedeemer \
    --protocol-params-file "$keypath/pparams.json" \
    --out-file "$keypath/delegate-staking-script.txbody"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/delegate-staking-script.txbody" \
    --signing-key-file "$keypath/bob.skey" \
    --out-file "$keypath/delegate-staking-script.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/delegate-staking-script.tx"


