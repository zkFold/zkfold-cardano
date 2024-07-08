#! /bin/bash

keypath=./keys

echo "Create stake pool."

mkdir -p $keypath

#----------------------------------- :.....: -----------------------------------

cardano-cli conway stake-address registration-certificate \
  --stake-verification-key-file stake.vkey \
  --key-reg-deposit-amt $(cardano-cli conway query gov-state --testnet-magic 4 | jq .currentPParams.stakeAddressDeposit) \
  --out-file registration.cert

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --witness-override 2 \
  --tx-in $(cardano-cli query utxo --address $(cat payment.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]') \
  --change-address $(cat payment.addr) \
  --certificate-file registration.cert \
  --out-file tx.raw

cardano-cli conway transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file payment.skey \
  --signing-key-file stake.skey \
  --testnet-magic 4 \
  --out-file tx.signed

cardano-cli conway transaction submit \
  --testnet-magic 4 \
  --tx-file tx.signed

cardano-cli conway node key-gen \
  --cold-verification-key-file cold.vkey \
  --cold-signing-key-file cold.skey \
  --operational-certificate-issue-counter-file opcert.counter

cardano-cli conway node key-gen-KES \
  --verification-key-file kes.vkey \
  --signing-key-file kes.skey

cardano-cli conway node key-gen-VRF \
  --verification-key-file vrf.vkey \
  --signing-key-file vrf.skey

cardano-cli conway stake-pool registration-certificate \
  --cold-verification-key-file cold.vkey \
  --vrf-verification-key-file vrf.vkey \
  --pool-pledge 9000000000 \
  --pool-cost 340000000 \
  --pool-margin 0.05 \
  --pool-reward-account-verification-key-file stake.vkey \
  --pool-owner-stake-verification-key-file stake.vkey \
  --testnet-magic 4 \
  --pool-relay-ipv4 <RELAY NODE PUBLIC IP> \
  --pool-relay-port <RELAY NODE PORT> \
  --out-file pool-registration.cert

cardano-cli conway stake-address stake-delegation-certificate \
  --stake-verification-key-file stake.vkey \
  --cold-verification-key-file cold.vkey \
  --out-file delegation.cert

cardano-cli conway transaction build \
  --testnet-magic 4 \
  --witness-override 3 \
  --tx-in $(cardano-cli query utxo --address $(cat payment.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]') \
  --change-address $(cat payment.addr) \
  --certificate-file pool-registration.cert \
  --certificate-file delegation.cert \
  --out-file tx.raw

cardano-cli conway transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file payment.skey \
  --signing-key-file cold.skey \
  --signing-key-file stake.skey \
  --testnet-magic 4 \
  --out-file tx.signed

cardano-cli conway transaction submit \
  --testnet-magic 4 \
  --tx-file tx.signed

cardano-cli conway stake-pool id \
  --cold-verification-key-file cold.vkey \
  --output-format bech32 \
  --out-file pool.id

slotsPerKESPeriod=$(cat shelley-genesis.json | jq -r '.slotsPerKESPeriod')
slotNo=$(cardano-cli query tip --testnet-magic 4 | jq -r '.slot')
kesPeriod=$((${slotNo} / ${slotsPerKESPeriod}))
cardano-cli conway node issue-op-cert \
  --kes-verification-key-file kes.vkey \
  --cold-signing-key-file cold.skey \
  --operational-certificate-issue-counter-file opcert.counter \
  --kes-period ${kesPeriod} \
  --out-file opcert.cert

#-------------------------------------------------------------------------------

#maybe remove
cardano-node run --topology topology.json \
  --database-path db \
  --socket-path node.socket \
  --shelley-kes-key kes.skey \
  --shelley-vrf-key vrf.skey \
  --shelley-operational-certificate opcert.cert \
  --port 3001 \
  --config config.json

cardano-cli conway query stake-snapshot \
  --testnet-magic 4 \
  --stake-pool-id <pool_id>

grep -e TraceForgedBlock 

#----------------------------------------------------------------------------------------
