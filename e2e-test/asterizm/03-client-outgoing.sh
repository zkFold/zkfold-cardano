#! /bin/bash

set -e
set -u
set -o pipefail

configpath=./assets/config.json
keypath=./keys

# Build an outgoing Asterizm message (Cardano -> other chain)
# Header structure (112 bytes total):
#   srcChainId  (8 bytes):  0x38 (Cardano)
#   srcAddress  (32 bytes): client address placeholder
#   dstChainId  (8 bytes):  0x01 (destination chain)
#   dstAddress  (32 bytes): 0x39d2ba91296029aFBE725436B4824cA803e27391
#   txId        (32 bytes): 0x02
# Payload: "Outgoing from Cardano!" in hex

srcChainId="0000000000000038"
srcAddress="00000000000000000000000039d2ba91296029afbe725436b4824ca803e27391"
dstChainId="0000000000000001"
dstAddress="00000000000000000000000039d2ba91296029afbe725436b4824ca803e27391"
txId="0000000000000000000000000000000000000000000000000000000000000002"
payload=$(echo -n "Outgoing from Cardano!" | xxd -p | tr -d '\n')

message="${srcChainId}${srcAddress}${dstChainId}${dstAddress}${txId}${payload}"

echo "Message: $message"
echo "Submitting outgoing message..."

cabal run zkfold-cli:asterizm -- client send \
  --core-config-file $configpath \
  --signing-key-file $keypath/client.skey \
  --client-vkey-file $keypath/client.vkey \
  --beneficiary-address $(cat $keypath/client.addr) \
  --message "$message"
